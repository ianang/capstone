library(stringr); library(data.table)
library(quanteda); library(wordcloud)
sums5 <- fread("sums5.csv"); sums4 <- fread("sums4.csv")
sums3 <- fread("sums3.csv"); sums2 <- fread("sums2.csv")
setkey(sums5, word1, word2, word3, word4, word5)
setkey(sums4, word1, word2, word3, word4)
setkey(sums3, word1, word2, word3)
setkey(sums2, word1, word2)

# sums5[.("telling","me","about","his")][order(-count)][1:5]
# sums4[.("me","about","his")][order(-count)][1:5]
# sums3[.("about","his")][order(-count)][1:5]

library(shiny)
shinyServer(function(input, output) {
    output$predicted <- renderText({
        tk <- str_replace_all(input$usertext, "[^a-zA-Z '-]", "")
        tk <- tokens(tk, remove_punct=T,
                     remove_symbols=T, remove_separators=T)
        tk <- unlist(tk)
        n <- length(tk)
        s5 <- sums5[.(tk[n-3],tk[n-2],tk[n-1],tk[n])][order(-count)]
        s4 <- sums4[.(tk[n-2],tk[n-1],tk[n])][order(-count)]
        s3 <- sums3[.(tk[n-1],tk[n])][order(-count)]
        s2 <- sums2[.(tk[n])][order(-count)]
        if (!is.na(s5$word5[1])) {s5$word5[1]} else {
        if (is.na(s5$word5[1]) && !is.na(s4$word4[1])) {s4$word4[1]} else {
        if (is.na(s5$word5[1]) && is.na(s4$word4[1]) && !is.na(s3$word3[1])) {s3$word3[1]} else {
        if (is.na(s5$word5[1]) && is.na(s4$word4[1]) && is.na(s3$word3[1]) && !is.na(s2$word2[1])) {s2$word2[1]} else {
        if (is.na(s5$word5[1]) && is.na(s4$word4[1]) && is.na(s3$word3[1]) && is.na(s2$word2[1])) {"*sorry, no prediction*"}}}}}
    })
    output$cloudcloud <- renderPlot({
        tk <- str_replace_all(input$usertext, "[^a-zA-Z '-]", "")
        tk <- tokens(tk, remove_punct=T,
                     remove_symbols=T, remove_separators=T)
        tk <- unlist(tk)
        n <- length(tk)
        s5 <- sums5[.(tk[n-3],tk[n-2],tk[n-1],tk[n])][order(-count)]
        s4 <- sums4[.(tk[n-2],tk[n-1],tk[n])][order(-count)]
        s3 <- sums3[.(tk[n-1],tk[n])][order(-count)]
        s2 <- sums2[.(tk[n])][order(-count)]
        if (!is.na(s5$word5[1])) {
            wordcloud(words=s5$word5, freq=s5$count, ordered.colors=TRUE)
        } else {
        if (is.na(s5$word5[1]) && !is.na(s4$word4[1])) {
            wordcloud(words=s4$word4, freq=s4$count, ordered.colors=TRUE)
        } else {
        if (is.na(s5$word5[1]) && is.na(s4$word4[1]) && !is.na(s3$word3[1])) {
            wordcloud(words=s3$word3, freq=s3$count, ordered.colors=TRUE)
        } else {
        if (is.na(s5$word5[1]) && is.na(s4$word4[1]) && is.na(s3$word3[1]) && !is.na(s2$word2[1])) {
            wordcloud(words=s2$word2, freq=s2$count, ordered.colors=TRUE)
        } else {
        if (is.na(s5$word5[1]) && is.na(s4$word4[1]) && is.na(s3$word3[1]) && is.na(s2$word2[1])) {}}}}}
    })
    output$plotplot <- renderPlot({
        tk <- str_replace_all(input$usertext, "[^a-zA-Z '-]", "")
        tk <- tokens(tk, remove_punct=T,
                     remove_symbols=T, remove_separators=T)
        tk <- unlist(tk)
        n <- length(tk)
        s5 <- sums5[.(tk[n-3],tk[n-2],tk[n-1],tk[n])][order(-count)]
        s4 <- sums4[.(tk[n-2],tk[n-1],tk[n])][order(-count)]
        s3 <- sums3[.(tk[n-1],tk[n])][order(-count)]
        s2 <- sums2[.(tk[n])][order(-count)]
        if (!is.na(s5$word5[1])) {
            barplot(s5$count ~ s5$word5)
        } else {
        if (is.na(s5$word5[1]) && !is.na(s4$word4[1])) {
            barplot(s4$count ~ s4$word4)
        } else {
        if (is.na(s5$word5[1]) && is.na(s4$word4[1]) && !is.na(s3$word3[1])) {
            barplot(s3$count ~ s3$word3)
        } else {
        if (is.na(s5$word5[1]) && is.na(s4$word4[1]) && is.na(s3$word3[1]) && !is.na(s2$word2[1])) {
            barplot(s2$count ~ s2$word2)
        } else {
        if (is.na(s5$word5[1]) && is.na(s4$word4[1]) && is.na(s3$word3[1]) && is.na(s2$word2[1])) {}}}}}
    })
})

# p <- ggplot(s5, aes(label=word5, size=count, color=word5))
# p <- p + geom_text_wordcloud_area()
# p <- p + scale_size_area(max_size=48)
# p <- p + theme_minimal()
# p

# p <- ggplot(data=s5, aes(x=word5,y=count))
# p <- p + geom_bar(stat="identity")
# p <- p + scale_x_discrete(limits=s5$word5) + coord_flip()