

bagels <- function(ndigits=3, nguesses = 8) {
library(shinyjs)
library(shiny)
library(shinyWidgets)


shinyApp(

  ui <- fluidPage(
    useShinyjs(),  # Set up shinyjs
    # function to track return press:
    tags$script('
                $(document).on("keyup", function(e) {
                if(e.keyCode == 13){
                Shiny.onInputChange("returnPressed", Math.random());  } }); '),
    # function to focus on selected shiny input
    tags$script('
                Shiny.addCustomMessageHandler("refocus",
                function(e_id) {
                document.getElementById(e_id).focus();
                });'),
      # adjust padding of text outputs to allign with inputs better
    tags$style(type='text/css', ".shiny-text-output {padding-top: 15px}" ),
     # adjust padding of well panel
    tags$style(type='text/css', ".well {padding-top: 0px; padding-bottom: 0px; }" ),
    # adjust color of texts
    tags$style(type='text/css', ".shiny-text-output {color: #38AFDA }" ),

         column(width = 6,
             dropdownButton(status = "info", label= "info", tooltip = F,  icon = icon("question"),
                            h5("pico:  a number is correct yet in the wrong position"),
                            h5("fermi:  a number is correct and in the correct position"),
                            h5("bagels: none of the numbers is correct"),
                            h3(" "),
                            h5("numbers: 0,1,2,3,4,5,6,7,8,9")),

         br(),
             wellPanel(
               h5(paste0("enter ", ndigits, " numeric values:")),

               # create 'nguesses' textInputs and outputs:
               lapply(1:nguesses, function(i) {
                 splitLayout(cellWidths = c("4%","22%", "10%", "65%"),
                             br(),
                             textInput(paste0('guess', i), "", placeholder = paste0(rep("x", ndigits)) ) ,
                             br(),
                             h5(textOutput(paste0('clue', i))) )})) )),


  server <- function(input, output, session) {
    gcurrent <- NULL
    resp <- NULL

    gameStart <- function(gcurrent, nguesses){

      for (n in 1: nguesses){
        disable(paste0("guess", n))
        disable(paste0("clue",n))
        updateTextInput(session, paste0("guess", n), label = NULL, value = "", placeholder = paste(rep("x", ndigits), collapse = ""))
        output[[paste0("clue", n)]] <-  renderText({""}) }

      gvector <<- as.character(sample(0:9,ndigits))
      print(gvector)
      enable("guess1")
      session$sendCustomMessage("refocus",list("guess1"))
      gcurrent <- 1
      gcurrent <<- gcurrent
      nextEnterStartGame <<- FALSE
      }


    gameAdvance <- function(resp, gcurrent, nguesses) {
      resp <- sort(resp, decreasing = T)
      resp <<- resp

      output[[paste0("clue", gcurrent)]] <- renderText({ paste0(resp, collapse = "")})  # feedback trough output clue with nubmergcurrent

      disable(paste0("guess", gcurrent))  # disable gcurrent guess

      if (gcurrent == nguesses) {
        output[[paste0("clue", gcurrent)]] <- renderText({paste0("nope... start again",  collapse = "")})
        nextEnterStartGame <<- TRUE }

      if (gcurrent < nguesses) {
        gcurrent <- gcurrent + 1
        gcurrent <<- gcurrent   # update gcurrent

        enable(paste0("guess", gcurrent ))  # enable new gcurrent
        session$sendCustomMessage("refocus",list( paste0("guess", gcurrent)))     # shift focus to new gcurrent
      }
      } # end of gameAdvance


    #####

    gameStart(gcurrent, nguesses)


    observeEvent(input$returnPressed, {

     if( nextEnterStartGame ==TRUE){
       gameStart(gcurrent, nguesses)
     }

      else if( nextEnterStartGame ==FALSE){

      #check if the entered string is the correct number of digits from start to end:
      if(!grepl(paste0("^[0-9]{", ndigits,"}$"), input[[paste0("guess", gcurrent)]][[1]]) ) {    }
      #if false do nothing


       if(grepl(paste0("^[0-9]{", ndigits, "}$"), input[[paste0("guess", gcurrent)]][[1]]) ) {
        #if true do *everything*: evaluate string, determine clue, return clue through advancegame() to next guess,
        # or play 'success' sequence followed by next gameStart().

        vguess <<- unlist(strsplit( input[[paste0("guess", gcurrent)]][[1]],""))

        fermivector <- vguess == gvector
        picovector <- vguess %in% gvector

        if (all(fermivector)) {
            output[[paste0("clue", gcurrent)]] <- renderText({ "Success!!"})
            nextEnterStartGame <<- TRUE
            }

        else if  ( !any(picovector)) {
          resp <- "bagels"
          gameAdvance(resp, gcurrent, nguesses) }

        else if (any(picovector) & !all(fermivector)) {
          resp <- c("", "", "")
          for (n in 1:ndigits){

            if(picovector[n] == T & fermivector[n] == F){
              resp[n] <-  "pico "
            }
            if(picovector[n] == T & fermivector[n] == T){
              resp[n] <- "fermi " }}

          gameAdvance(resp, gcurrent, nguesses) }

    }}
    })
}) # end of server


# Run the application
shinyApp(ui = ui, server = server)

}

#bagels()
