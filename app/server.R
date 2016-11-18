# server.R
source("helpers.R")

#Classify Movie Review Text

shinyServer(function(input, output) {
    output$value <- renderText({        
        it_test_case = itoken(input$text, 
                              preprocessor = tolower, 
                              tokenizer = stem_tokenizer, 
                              progressbar = FALSE)
        dtm_test_case = create_dtm(it_test_case,vectorizer)
        dtm_test_case_binary = sign(dtm_test_case)
        dtm_test_case_tfidf  = dtm_test_case %>% transform(tfidf)
        preds_test_case = predict(model,dtm_test_case_tfidf, type = 'response')[,1]
        paste0("Probaility for a good review = ", round(preds_test_case*100,2),"%")
    })
    }
)
