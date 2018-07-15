server <- function(input, output) {
  
  math_wrap <- function(id,text,...){
    output[[id]] <- renderUI(
      tagList(
        list(...),
        withMathJax(
          text
        )
        
        
      )
    )
  }
  
  math_wrap("vector_basic_math_title", 
            h2("$$\\vec{v} \\text{ is the vector addition of the scaled basis vectors: }\\hat{i} \\text{,} \\hat{j}$$"))
  
  math_wrap("vector_basic_math_v",
            h3("$$\\vec{v} = $$"),
            br(),
            br())
  
  output$vector_basic_math_vector_text <- renderUI({
    i <- input$vector_basic_v1
    j <- input$vector_basic_v2
    tagList(
      br(),
      br())
      withMathJax(
        h3(paste0("$$\\text{} \\vec{v}= ",i,"\\hat{i}\\text{ + }",j,"\\hat{j}
                      \\text{ = }",i,"\\begin{bmatrix} 1 \\\\ 0 \\end{bmatrix} \\text{ + }",
                  j,"\\begin{bmatrix} 0 \\\\ 1 \\end{bmatrix} \\text{ = } 
                  \\begin{bmatrix}",i,"\\\\",j,"\\end{bmatrix}$$")
      )
      
    )
  })
  
  
  ggplot_coord_axes_theme <- function(){
    list(geom_vline(xintercept = 0,linetype = "dotted"), geom_hline(yintercept = 0,linetype = "dotted"), theme_minimal())
  }
  
  make_A <- function(x,...) UseMethod("make_A")
  make_A.eigen <- function(x,...){
    A <-  x$vectors
    
    structure(A, class = c("eigen","matrix"))
  }
  
  make_A.default <- function(x,...){
    
    A <- unlist(list(x,...))
    A <- matrix(A,2,2)
    
    if (is.na(A[1,1])){
      A <- matrix(c(1.2,-1,.6,-1),nrow=2)
    }
    
    structure(A, class = c("animate_matrix","matrix"))
  }
  
  make_v <- function(x,...) UseMethod("make_v")
  
  make_v.default <- function(x,...) {
    v <- unlist(list(x,...))
    v <- matrix(v,nrow = 2)
    if (length(v) == 0){
      v <- matrix(c(4,6))
    }
    structure(v,class = c("animate_vector","matrix"))
    
  }
  
  
  split_into_frames_df <- function(x,...) UseMethod("split_into_frames_df")
  
  split_into_frames_df.animate_matrix <- function(A,n_frames = 10){
    el1 <- seq(from = 1, to = A[1,1], length.out = n_frames)
    el2 <- seq(from = 0, to = A[2,1], length.out = n_frames)
    el3 <- seq(from = 0, to = A[1,2], length.out = n_frames)
    el4 <- seq(from = 1, to = A[2,2], length.out = n_frames)
    init <- replicate(n_frames * 2,0)
    df <- data.frame(x = 0, xend = init, y = 0, yend = init,frames = 0)
    
    for (i in 1:n_frames){
      new_A <- matrix(c(el1[i],el2[i],el3[i],el4[i]),2,2)
      row_vec <- c(i * 2 - 1,i * 2)
      df[row_vec,] <- data.frame(x = 0, xend = c(new_A[1,1],new_A[1,2]), y = 0, yend = c(new_A[2,1],new_A[2,2]),frames = i)
    }
    df
    
  }
  
  split_into_frames_df.animate_vector <- function(v,A = NULL,n_frames = 10,add = F, v2 = NULL, static = F){
    if (static){
      return(data.frame(x = rep(x = 0, n_frames), xend = v[1,1],y = 0, yend = v[2,1], frames  = 1))
    }
    
    if (!add){
      
      if (is.null(A)){
        el1 <- seq(from =0, to = v[1,1], length.out = n_frames)
        el2 <- seq(from = 0, to = v[2,1], length.out = n_frames)
        
      }else{
        transformed_v <- A %*% v
        el1 <- seq(from = v[1,1], to = transformed_v[1,1], length.out = n_frames)
        el2 <- seq(from = v[2,1], to = transformed_v[2,1], length.out = n_frames)
        
      }
      

      init <- replicate(n_frames,0)
      df <- data.frame(x = 0, xend = init, y = 0, yend = init,frames = 0)
      
      for (i in 1:n_frames){
        new_A <- matrix(c(el1[i],el2[i]),nrow = 2)
        df[i,] <- data.frame(x = 0, xend = new_A[1,1], y = 0, yend = new_A[2,1],frames = i)
      }
      
    }else{
      transformed_v <- v + v2
      df <- data.frame(
        x = seq(from = 0, to = v2[1,1], length.out = n_frames),
        y = seq(from = 0, to = v2[2,1], length.out = n_frames),
        xend = seq(from = v[1,1], to = transformed_v[1,1], length.out = n_frames),
        yend = seq(from = v[2,1], to = transformed_v[2,1], length.out = n_frames),
        frames = 1:n_frames
      )
      # el1 <- seq(from = v[1,1], to = transformed_v[1,1], length.out = n_frames)
      # el2 <- seq(from = v[2,1], to = transformed_v[2,1], length.out = n_frames)
      # 
      # init <- replicate(n_frames,0)
      # df <- data.frame(x = v[1,1], xend = init, y = v[2,1], yend = init,frames = 0)
      # 
      # for (i in 1:n_frames){
      #   new_A <- matrix(c(el1[i],el2[i]),nrow = 2)
      #   df[i,] <- data.frame(x = v[1,1], xend = new_A[1,1], y = v[2,1], yend = new_A[2,1],frames = i)
      # }
    }

    df
    
    
  }

  
  combine_frames_by_section <- function(frames_df,sections,text = "",n_frames = 10){

    
    if (length(frames_df) != length(sections)) stop("Number of sections must equal the number of frame data frames passed")
     number_of_sections <- length(unlist(sections))

    df <- data.frame(x = integer(0),xend = integer(0), y = integer(0), yend = integer(0), frames = integer(0),text = integer(0))
    
    for (i in 1:length(sections)){
        current_section <- sections[[i]]
        for (j in 1:length(current_section)){
          current_df <- frames_df[[i]]
          current_df$frames <- ((current_section[j] - 1)*n_frames + 1):((current_section[j] - 1)*n_frames + 10)
          
          if (!is.null(text)) current_df$text <- text[i]
          
          df <- rbind(df, current_df)
          
        }
      
    }
    
    
    # df <- do.call(rbind,list(...))
    # df$text <- rep(text,each = nrow(df) / length(text))
    df
  }

  
  observeEvent(input$vector_basic_plot_button,{
    output$vector_basic_plot <- renderPlotly({
        #browser()
      
      vx <- as.numeric(input$vector_basic_v1)
      vy <- as.numeric(input$vector_basic_v2)
      
      
      vec_i <- make_v(c(1,0))    
      vec_j <- make_v(c(0,1))
      vec_new <- make_v(c(vx,vy))
      
      A_i <- make_A(c(vx,0,0,vx))
      A_j <- make_A(c(vy,0,0,vy))
    
      transformed_i_frames <- split_into_frames_df(vec_i,A = A_i)
      transformed_j_frames <- split_into_frames_df(vec_j,A = A_j)
 
      static_transformed_j <- split_into_frames_df(vec_j * vy, static = T)
      
      add_vi_df <- split_into_frames_df(vec_i * vx, v2 = vec_j * vy , add = T)
      
      transformed_new_frames <- split_into_frames_df(vec_new)  

      frame_df <- combine_frames_by_section(list(transformed_i_frames,
                                                 transformed_j_frames,
                                                 static_transformed_j,add_vi_df,transformed_new_frames),
                                            list(1,1,2:3,2:3,3),text = c("i","j","j","add","transformed"))


      g <- ggplot(data = frame_df, aes(x = x, y = y, xend = xend, yend = yend, frame = frames,label = text)) +geom_segment()+ geom_text() +
        ggplot_coord_axes_theme()
      
      ggplotly(g)
    })
    
    
    
  })

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

}
