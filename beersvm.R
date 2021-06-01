library(tidyverse)
library(e1071)

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

ggplot(pa, aes(abv, ibu, color=style)) +
  geom_point()

pa_train <- seq_len(nrow(pa)) %in% sample(seq_len(nrow(pa)), round(0.6*nrow(pa)))
pa_test <- !pa_train

pa_linear <- tune(svm, style ~ abv + ibu, data=pa[pa_train,], scale=FALSE, kernel = 'linear', ranges=list(cost = seq(0.1, 10, by=0.1)))
pa_radial <- tune(svm , style ~ abv + ibu, data=pa[pa_train,], gamma=1, scale=FALSE, kernel='radial', ranges=list(cost = seq(0.1, 10, by=0.1)))

pa_lin_best <- pa_linear$best.model
pa_rad_best <- pa_radial$best.model

pred_grid <- expand.grid(abv = seq(min(pa$abv), max(pa$abv), length.out=500),
                         ibu = seq(min(pa$ibu), max(pa$ibu), length.out=500)) %>%
  mutate(linear = predict(pa_lin_best, ., type='class'),
         radial = predict(pa_rad_best, ., type='class'))

pred_grid %>%
  gather(model, prediction, -abv, -ibu) %>%
  ggplot() +
  geom_tile(aes(abv, ibu, fill=prediction), alpha=0.5) +
  geom_point(aes(abv, ibu, color=style), data=pa[pa_train,]) +
  facet_grid(.~model)


lp <- beer %>%
  filter(style=='Lager' | style=='Porter')
lp$style <- factor(lp$style)

svm_lp <- svm(style ~ abv + ibu, lp, kernel='linear', type='C-classification', cost=10)
pred_lp <- predict(svm_lp)

svm_ales <- svm(style ~ abv+ibu, ales, kernel='polynomial', type='C-classification', cost=10)
pred_ales <- predict(svm_ales)
