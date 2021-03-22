#install.packages("radarchart")

library(radarchart)

emotions_diff <- emotions  %>%
  left_join(overall_mean_sd, by="sentiment") %>%
  mutate(difference=percent-overall_mean)

scores <- data.frame("Label"=c("anger", "anticipation", "disgust",
                               "fear",  "joy", "sadness", "surprise", "trust"),
                     "Harry" = c(9,13,8,17,11,10,11,21),
                     "Draco" = c(9,20,9,14,10,9,10,17),
                     "Dobby" = c(19,9,7,12,7,17,7,22),
                     "Tom Riddle" = c(9,12,6,16, 14,12,8,14),
                     "Lockhart" = c(12,18,9,13,11,12,11,14)
                     )
color <- grDevices::col2rgb(c("red", "blue", "green", "magenta", "cyan", "orange", "purple", "pink"))
chartJSRadar(scores[,c(1,6)], maxScale = 25, showToolTipLabel=TRUE, main = "Characters Sentiment", colMatrix = color)
