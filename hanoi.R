library(dplyr)
library(ggplot2)
library(purrr)
toa <- R6::R6Class(
    classname = "toa",
    # values
    public = list(
        pieces = list(
            "A" = list(),
            "B" = list(),
            "C" = list()
        ),
        n_piecies = 0,
        colors = RColorBrewer::brewer.pal(10,"Set3"),
        solution = c(),
        solution_l = list(),
        current_step = 1,
        # public methods
        initialize = function(number_of_pieces){
            stopifnot(is.integer(number_of_pieces))
            stopifnot(number_of_pieces > 0 & number_of_pieces <= 10)
            self$pieces$A <- purrr::map(
                1:number_of_pieces,~list(size = .x,color = self$colors[.x])
            ) %>% setNames(1:number_of_pieces)
            self$n_piecies = number_of_pieces
            self$solution = self$steps()
            self$solution_l <- purrr::map(self$solution, function(string){
                from <- stringr::str_extract(string,"(?<=from ).(?= to)")
                to <- stringr::str_extract(string,"(?<=to ).$")
                return(c(from = from,to = to))
            })
            invisible(self)
        },
        piece_move_to = function(f,t){
            top_piece_index <- which.min(as.integer(names(self$pieces[[f]])))
            piece <- self$pieces[[f]][top_piece_index]
            # remove piece from f
            self$pieces[[f]] <- self$pieces[[f]][-top_piece_index]
            # move piece to t
            self$pieces[[t]] <- append(self$pieces[[t]],piece)
            invisible(self)
        },
        steps = function(){
            return(
                capture.output(
                    private$move_via_to("A","B","C",self$n_piecies)
                )
            )
        },
        advance = function(){
            if(self$current_step <= length(self$solution)){
                next_step <- self$solution_l[[self$current_step]]
                self$piece_move_to(next_step["from"],next_step["to"])
                self$current_step <- self$current_step + 1
            }
            invisible(self)
        },
        snapeshot = function(){
            width_multiplier <- 0.04
            state = testdf <- 
                purrr::map2_df(self$pieces,names(self$pieces), function(pieces,name){
                purrr::map_df(pieces,function(x){
                    tibble::tibble(
                        size = x$size,
                        fill = x$color,
                        color = "black"
                    ) 
                }) %>%
                        dplyr::arrange(size) %>%
                        dplyr::mutate(
                            pillar = name,
                            x = dplyr::case_when(
                                name == "A" ~ 1,
                                name == "B" ~ 2,
                                name == "C" ~ 3
                            ),
                            y = rev(dplyr::row_number())
                        )
            })
            p <- state %>%
                ggplot() +
                geom_segment(aes(x = 1,xend = 1, y = -0.1, yend = 10),size = 2) +
                geom_segment(aes(x = 2,xend = 2, y = -0.1, yend = 10),size = 2) +
                geom_segment(aes(x = 3,xend = 3, y = -0.1, yend = 10),size = 2) +
                geom_hline(yintercept = -0.1, size = 3,) +
                geom_text(
                    aes(x = x, y = -1, label = labels),
                    data = tibble::tibble(
                        labels = c("A","B","C"),
                        x = c(1,2,3)
                    ),
                    size = 8
                ) + 
                geom_rect(
                    aes(
                        xmin = x - width_multiplier * size,
                        xmax = x + width_multiplier * size,
                        ymin = y-1,
                        ymax = y,
                        fill = fill
                    ),
                    color = "black"
                ) +
                coord_fixed(ratio = 0.1) +
                xlim(c(0.5,3.5)) +
                ylim(c(-1,11)) +
                theme_void() +
                guides(fill = FALSE)
            return(p)
        }
    ),
    private = list(
        move_to = function(f,t){
            cat(sprintf("move from %s to %s\n",f,t))
        },
        move_via_to = function(f,v,t,n){
            if(n > 1) {
                private$move_via_to(f,t,v,n-1)
                private$move_to(f,t)
                private$move_via_to(v,f,t,n-1)
            } else {
                private$move_to(f,t)
            }
        }
    )
)

