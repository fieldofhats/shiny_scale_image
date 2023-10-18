packs<- c('shiny', 'magick', 'tidyverse', 'colourpicker', 'shinydashboard', 'shinyFiles')

## alternate cran mirror...
# setRepositories(addURLs =
#                     c(CRANxtras = "https://ftp.osuosl.org/pub/cran/")) 


for(i in 1:length(packs)){
  if (!require(packs[i], character.only = T)){
    install.packages(packs[i])
    require(packs[i], character.only = T)
  }
}

css.point.text <- tags$head(
  tags$style(
    HTML(
      "
        #measure_pts {
        width: 300px; 
        max-width: 100%;
        padding: 12px 12px;
        }
        "
    )
  )
)

css.img_data.text <- tags$head(
  tags$style(
    HTML(
      "
        #photo_info {
        width: 325px; 
        max-width: 100%;
        padding: 12px 12px;
        }
        "
    )
  )
)



  
header <- dashboardHeader(
                          title = "Image Scale Calculations",
                          titleWidth = 400)

sidebar <- dashboardSidebar(
  
  #tags$head(tags$style(css)),
  
  width = 400,
  
  menuItem('Image',
           h4('open Image:'),
           fileInput("upload", "Upload Wildlife image", accept = c('image/png', 'image/jpeg')),
           fileInput("upload_scale", "Upload scale image", accept = c('image/png', 'image/jpeg')),
           #textInput("size", "Size", value = "500x500"),
           h4('scale image size'),
           sliderInput('pct', 'percent size', 1 , 100, 50),
           h4('overlay scale image'),
           checkboxInput('overlay_add', 'add overlay', FALSE),
           sliderInput("overlay", "percent overlay", 0, 100, 0),
           # sliderInput("blur", "Blur", 0, 20, 0),
           #sliderInput("implode", "Implode", -1, 1, 0, step = 0.01),
           h4('Effects'),
           checkboxGroupInput("effects", NULL,
                              choices = list("negate", "charcoal", "edge"), inline = T)
  ),
  # menuItem('calcs', 
  #          h3('Scale Calcs:'),
  #          textInput('camH', 'Camera Height', value = 0),
  #          textInput('camD', 'Depth Distance', value = 300)
  #          
  # ),
  menuItem('scale/measure',
           
           h4('Scale Calcs:'),
           textInput('camH', 'Camera Height', value = 0),
           textInput('camD', 'Depth Distance', value = 300),
           colourInput("color_in", "scale-bar color", "yellow"),
          
           menuItem('Measure',
                    css.point.text,
                    br(),
                    verbatimTextOutput("measure_pts", placeholder = T),
                    br(),
                    # verbatimTextOutput("measure_pts", placeholder = T),
                    #verbatimTextOutput("measure_pts_2", placeholder = T),
                    checkboxInput('h_measure', 'Horizontal ruler', FALSE),
                    checkboxInput('v_measure', 'vertical ruler', FALSE)
                    #actionButton('measure_now', 'Measure')
           ),
           menuItem('add scale bar',
                        #h4('Add scale Bar:'),
             
             # p(style = 'text-align: center;',
             #   h5(''),
             br(),
             textInput('h_line', 'horizontal scale cm', value = 50),
             textInput('v_line', 'vertical scale cm', value = 50),
             checkboxInput('h_line_add', 'add H scale at coordinate', FALSE),
             sliderInput("h_adjust", "Horizontal bar adjustment", 1, 100, 10),
             checkboxInput('v_line_add', 'add V scale at coordinate', FALSE),
             sliderInput("v_adjust", "vertical bar adjustment", 1, 100, 10)

          ),
          menuItem('annotate',
                   checkboxInput('annotate', 'annotate image data', F),
                   radioButtons("an_location", 
                                      NULL,
                                      choices = list("top_left"='northwest', "top_right"='northeast', "bottom_right"='southeast', 'bottom_left'='southwest'),
                                      selected = 'southwest',
                                      inline = T)
                   
                   )

  ),
  menuItem('save current image',
           shinySaveButton("save", "Save file", "Save file as ...", filetype=list(jpg="jpg"))   
  ),
  menuItem('image data',
           css.img_data.text,
            h3('image data: '),
            verbatimTextOutput("photo_info", placeholder = T)    
  )


  
)

body <- dashboardBody(
  
    imageOutput("img", 
                click = 'image_click')
)
  
  


ui <- dashboardPage(header, sidebar, body)

server <- function(input, output, session) {
  
  library(magick)
  
  # Start with placeholder img
  image <- image_read("./images/18511_3_20230615_211945_01_3176.JPG")
  image_scale_overlay <- image_read("./images/18511_3_20230509_163923_10_0010.JPG")
  image_write(image, './tmp/tmpImage.jpg', format = 'jpg' )
  
  original_dims <-c( image_info(image)$width, image_info(image)$height)
  
  # upload a new wildlife image
  observeEvent(input$upload, {
    if (length(input$upload$datapath))
      image <<- image_read(input$upload$datapath)
    info <- image_info(image)
    updateCheckboxGroupInput(session, "effects", selected = "")
    #updateTextInput(session, "size", value = paste(info$width, info$height, sep = "x"))
  })
  
  #upload a new scale image
  observeEvent(input$upload_scale, {
    if (length(input$upload_scale$datapath))
      image_scale_overlay <<- image_read(input$upload_scale$datapath)
    info_scale <- image_info(image_scale_overlay)
    #updateCheckboxGroupInput(session, "effects", selected = "")
    #updateTextInput(session, "size", value = paste(info$width, info$height, sep = "x"))
  })
  
  
  original_dims <-c( image_info(image)$width, image_info(image)$height)
  
  ## exponential decreasing function from measured data..
  ## full sized photo.. 2048 px wide (need to scale with photo)
  
  dist <- reactive(sqrt((as.numeric(input$camH))^2 + (as.numeric(input$camD))^2)) 
  
  fn_dat <- data.frame( c = 3.433659, d = 52.085851, e = 148.335057 )
  px_per_cm_fn = function(x) fn_dat$c +(fn_dat$d - fn_dat$c) * exp(-x/fn_dat$e)
  px_per_cm<- reactive(px_per_cm_fn(dist()))
  
  
  
  hline <- reactive(as.numeric(input$h_line)*px_per_cm())
  vline <- reactive(as.numeric(input$v_line)*px_per_cm())
  
 
    

  
  ## save dataframe of clicks
  click_dat <- reactiveVal(NULL)
  click_dat(data.frame(x = 0, y = 0))
  observeEvent(c(input$image_click$x, input$image_click$y), {
    # if (is.null(click_dat())) {
    #   click_dat(data.frame(x = input$image_click$x, y = input$image_click$y))
    # } else {
    #   click_dat(rbind(click_dat(),
    #                   data.frame(x = input$image_click$x, y = input$image_click$y)))
    # }
    click_dat(rbind(click_dat(),
                      data.frame(x = input$image_click$x, y = input$image_click$y)))
  })
  
  # A plot of fixed size
  output$img <- renderImage({
    
    
    
    # Boolean operators
    if("negate" %in% input$effects)
      image <- image_negate(image)
    
    if("charcoal" %in% input$effects)
      image <- image_charcoal(image)
    
    if("edge" %in% input$effects)
      image <- image_edge(image)
    
    # if(input$h_line_add){
    #   image_draw(image)
    #   rect(input$image_click$x, 
    #        input$image_click$y, 
    #        input$image_click$x + input$h_line, 
    #        input$image_click$y + 10, 
    #        border = 'yellow', 
    #        fill = 'yellow',
    #        lwd = 1)
    #   #print(image)
    # }
    if(input$h_line_add){
      #color_in <- 'yellow'
      h_yellow_line <- reactive(image_blank(width = hline(), height = 10, color = input$color_in))
      #tab<-isolate(click_dat())
      tab<-click_dat()
      x<-tab[nrow(tab),1]  * 1/(input$pct*.01)
      y<-tab[nrow(tab),2]  * 1/(input$pct*.01)
      
      
      ## output for info text below..
      x.out<<-tab[nrow(tab),1]  * 1/(input$pct*.01)
      y.out<<-tab[nrow(tab),2]  * 1/(input$pct*.01)
      
      ## h adjust:
      y_adj <- seq(y + y*.1, y - vline() - y*.1,   length.out = 100)
      y1 <- y_adj[input$h_adjust]
      
      image <- image_composite(image, 
                               h_yellow_line(),   
                               offset = paste0(geometry_point(x,y1) ))
      image <- image_annotate(image, 
                              paste0(input$h_line, ' cm'), 
                              color = input$color_in, 
                              size = 40 *  (.3 + .0075*input$pct), 
                              location = geometry_point( x + hline()/2*.7, y1 + 15) ) 
    }
    
    if(input$v_line_add){
      #color_in <- 'yellow'
      v_yellow_line <- reactive(image_blank(width = 10, height = vline(), color = input$color_in))
      #tab<-isolate(click_dat())
      tab<-click_dat()
      x<-tab[nrow(tab),1]  * 1/(input$pct*.01)
      y<-tab[nrow(tab),2]  * 1/(input$pct*.01)
      
      
      ## v adjust:
      x_adj <- seq(x - x*.1, x + hline() + x*.1,   length.out = 100)
      x1 <- x_adj[input$v_adjust]
      
      image <- image_composite(image, 
                               v_yellow_line(),   
                               offset = paste0(geometry_point(x1 - 20,y - vline()) ))
      image <- image_annotate(image, 
                              paste0(input$v_line, '\ncm'), 
                              color = input$color_in, 
                              size = 40 *  (.3 + .0075*input$pct), 
                              location = geometry_point( x1-60, y +( (vline()/2*.7)) - vline() ))
    }
    
    
    
   
 ###########################
    ###    measure h and v:
    if(input$h_measure | input$v_measure ){ 
      pt.tab <- click_dat()
      
      
      # if no clicks yet, use 0,0 coord
      if(nrow(pt.tab) < 2){
        pt.1 <-  c(0,0)
        pt.2 <-  c(0,0)
      }else{
          pt.1 <-  pt.tab[nrow(pt.tab)-1,]
          pt.2 <-  pt.tab[nrow(pt.tab),]
      }
      
      
      x.m.1 <- pt.1[1] * 1/(input$pct*.01)
      y.m.1 <- pt.1[2] * 1/(input$pct*.01)
      x.m.2 <- pt.2[1] * 1/(input$pct*.01)
      y.m.2 <- pt.2[2] * 1/(input$pct*.01)
      if(x.m.1 > x.m.2){
        ## swap vars:
        #a  <- b - a + (b <- a)
        x.m.1 <- x.m.2 - x.m.1 + (x.m.2 <- x.m.1)
      }
      if(y.m.1 > y.m.2){
        ## swap vars:
        #a  <- b - a + (b <- a)
        y.m.1 <- y.m.2 - y.m.1 + (y.m.2 <- y.m.1)
      }
      
      x.dist <- abs(x.m.1 - x.m.2)
      y.dist <- abs(y.m.1 - y.m.2)
      x.cm <- x.dist/px_per_cm()
      y.cm <- y.dist/px_per_cm()
      

      
      output$measure_pts <- renderPrint({
        cat('x:', round(x.m.1[[1]],0),  
            ',', round(x.m.2[[1]],0),
            ' x.pix:', round(x.dist[[1]],0),
            ' x.cm:', round(x.cm[[1]],0),
            ' diag.cm:',  round(sqrt(x.cm[[1]]^2 + y.cm[[1]]^2),0),
            '\n',
            sep = ''
            )
        cat('y:',round(y.m.1[[1]],0) ,
            ',', round(y.m.2[[1]],0),
            ' y.pix:', round(y.dist[[1]],0),
            ' y.cm:', round(y.cm[[1]],0),
            '\n',
            sep = ''
            )
      }) 
      
      if(input$h_measure){
          h_m_line <- reactive(image_blank(width = x.dist, height = 10, color = input$color_in))
          image <- image_composite(image, 
                       h_m_line(),   
                       offset = paste0(geometry_point(x.m.1, y.m.1) ))
          image <- image_annotate(image, 
                      paste0(round(x.cm,0), ' cm'), 
                      color = input$color_in, 
                      size = 40 *  (.3 + .0075*input$pct), 
                      location = geometry_point(x.m.1+x.dist/2*.7, y.m.1 + 15) )
      }   
      
      if(input$v_measure){
        v_m_line <- reactive(image_blank(height = y.dist, width = 10, color = input$color_in))
        image <- image_composite(image, 
                                 v_m_line(),   
                                 offset = paste0(geometry_point(x.m.1, y.m.1) ))
        image <- image_annotate(image, 
                                paste0(round(y.cm,0),'\n', 'cm'), 
                                color = input$color_in, 
                                size = 40 *  (.3 + .0075*input$pct), 
                                location = geometry_point(x.m.1-40,  y.m.1+y.dist/2*.7) )
      }
        
    }
      
      if(input$annotate){
        
            # img_tmp_info <<- reactive(image_info( image_read('../data/out/tmpImage.jpg')))
            # current_dims <<- reactive(round(original_dims * input$pct*.01, 0))
            # 
            # locs <- reactive(list(top_left = c(30,30), 
            #             top_right = c(current_dims()[2] - 20  , 30), 
            #             bottom_right = c(current_dims()[2] - 50, current_dims()[2] - 50), 
            #             bottom_left =  c(30, current_dims()[1] - 20)))
            # 
            # loc <<- reactive(locs()[[as.numeric(input$an_location)]])
            loc = input$an_location
            
            an.data <- paste0(
              'image data:\n',
              'dimensions: ',
              #round(img_info()$width  * input$pct*.01,0), 
              img_info()$width,
              ' x ',
              #round(img_info()$height * input$pct*.01,0),
              img_info()$height,
              '\n',
              'subject dist: ', dist(), '\n',
              'pix/cm: ', px_per_cm()
              
            )
            
            image <- image_annotate(image , 
                                    an.data, 
                                    color = input$color_in, 
                                    size = 40 *  (.3 + .0075*input$pct), 
                                    #location = geometry_point(loc()[1],  loc()[2] )
                                    gravity = loc,
                                    location = '+50+50'
            )
      
      
      
      
      
   }
    
 
  

    
    
    # 
    # if("flop" %in% input$effects)
    #   image <- image_flop(image)
    
    
    # Numeric operators
    tmpfile <- image %>%
      #image_rotate(input$rotation) %>%
      # image_implode(input$implode) %>%
      # image_blur(input$blur, input$blur) %>%
      #image_composite(image_overlay, operator = "blend", compose_args=paste0(image$overlay)) %>% 
      #image_resize(input$size) %>%
      image_scale(geometry_size_percent(width = input$pct, height = NULL)) %>% 
      image_write('./tmp/tmpImage.jpg', format = 'jpg')
    
    
    if(input$overlay_add){
      # fig <- image_graph(width = image_info(image)$width, height = image_info(image)$height)
      # print(image_overlay)
      tmpfile <- image %>% 
        image_composite(image_scale_overlay, operator = "blend", compose_args=paste0(input$overlay)) %>% 
        image_scale(geometry_size_percent(width = input$pct, height = NULL)) %>%
        image_write('./tmp/tmpImage.jpg', format = 'jpg')
    }
    
    
    #tmp_image <<- image_read('../data/out/tmpImage.jpg')
    # Return a list
    list(src = tmpfile, contentType = "image/jpeg")
    
  })
  
  tmp_image <- reactiveFileReader(1000, NULL, './tmp/tmpImage.jpg', image_read)
  img_info <- reactive(image_info(tmp_image()))
  img_info <- reactive(image_info(image_read('./tmp/tmpImage.jpg')))
  
  # x<-click_dat()[nrow(tab),1]  * 1/(input$pct*.01)
  # y<-click_dat()[nrow(tab),2]  * 1/(input$pct*.01)
  
  # coords<-data.frame(x = c(0,0), y = c(0,0))
  output$photo_info <- renderPrint({
    
   cat(sep = '',
     'current image dimensions:  ',
     #round(img_info()$width  * input$pct*.01,0),
     img_info()$width,
     ' x ',
     #round(img_info()$height * input$pct*.01,0), 
     img_info()$height,
       '\n'
       )
    
    cat(sep = '',
        "Current coordinate:  ")
    
    if(is.null(input$image_click$x)){
      cat(sep = '',
          '0, 0\n')
    }else{
      cat(sep = '',
          round(input$image_click$x,0),
          ', ', 
          input$image_click$y, '\n')
    }
      
    cat(sep = '',
      'calculated distance: ', dist(), '\n',
      'pix per cm: ', px_per_cm()
    )
    
    
  })
 
  
  observeEvent(input$save, {
    image.out <- image_read('./tmp/tmpImage.jpg')
    volumes <- c("UserFolder"="../")
    shinyFileSave(input, "save", roots=volumes, session=session)
    fileinfo <- parseSavePath(volumes, input$save)
    # data <- data.frame(a=c(1,2))
    if (nrow(fileinfo) > 0) {
      image_write(image.out, as.character(fileinfo$datapath), format = 'jpg')
    }
  })
  
  
  
}

shinyApp(ui, server)


