library(tidyverse)
library(e1071)

setwd('C:/Users/rvolk/Desktop/DSCI 320/Project')

beer <- read_csv('beers.csv')
beer <- beer[,-1]
beer <- beer %>% filter(!is.na(ibu))

for(i in 1:length(beer$style)) {
  style <- beer$style[i]
  
  if( grepl('IPA', style) ) {
    beer$style[i] <- 'IPA'
  } else if ( grepl('Lager', style) ) {
    beer$style[i] <- 'Lager'
  } else if ( grepl('Pilsner', style) | grepl('Pilsener', style) ) {
    beer$style[i] <- 'Pilsner'
  } else if ( grepl('Porter', style) ) {
    beer$style[i] <- 'Porter'
  } else if ( grepl('Stout', style) ) {
    beer$style[i] <- 'Stout'
  } else if ( grepl('APA', style) ) {
    beer$style[i] <- 'APA'
  } else if ( grepl('Pale Ale', style) ) {
    beer$style[i] <- 'Pale Ale'
  } else if ( grepl('Wheat Ale', style) ) {
    beer$style[i] <- 'Wheat Ale'
  } else if ( grepl('Red Ale', style) ) {
    beer$style[i] <- 'Red Ale'
  } else if ( grepl('Brown Ale', style) ) {
    beer$style[i] <- 'Brown Ale'
  } else if ( grepl('Ale', style) ) {
    beer$style[i] <- 'Misc. Ale'
  } else beer$style[i] <- 'Other'
  
}

ales <- beer %>% filter(grepl('Ale', style) &
                          style!='Pale Ale' &
                          style!='Misc. Ale')

beer$style <- as.factor(beer$style)
ales$style <- as.factor(ales$style)

svm_full <- svm(style ~ abv + ibu, beer, kernel = 'radial', type = 'C-classification', cost=10)
pred_full <- predict(svm_full)

pa <- beer %>%
  filter(style=='IPA' | style=='APA' | style=='Pale Ale')
pa$style <- factor(pa$style)

svm_pa <- svm(style ~ abv + ibu, pa, kernel = 'linear', type = 'C-classification', cost=10)
pred_pa <- predict(svm_pa)

lp <- beer %>%
  filter(style=='Lager' | style=='Porter')
lp$style <- factor(lp$style)

svm_lp <- svm(style ~ abv + ibu, lp, kernel='linear', type='C-classification', cost=10)
pred_lp <- predict(svm_lp)

svm_ales <- svm(style ~ abv+ibu, ales, kernel='polynomial', type='C-classification', cost=10)
pred_ales <- predict(svm_ales)
