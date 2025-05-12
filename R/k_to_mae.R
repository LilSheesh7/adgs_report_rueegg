library(caret)
library(recipes)
library(rsample)
library(yardstick)

# Funktion that takes k as an input and gives aout 
fit_and_eval_knn <- function(k_value, df) {

    # Fit KNN
  mod <- train(
    pp_2,
    data = daily_fluxes_train_2 |> drop_na(),
    method = "knn",
    tuneGrid = data.frame(k = k_value),
    trControl = trainControl(method = "none")
  )
  
  # add predictions to the data frames
  daily_fluxes_test_2 <- daily_fluxes_test_2 |> drop_na()
  daily_fluxes_test_2$pred <- predict(mod, newdata = daily_fluxes_test_2)
  
  # calculate MAE
  mae <- yardstick::mae_vec(daily_fluxes_test_2$GPP_NT_VUT_REF, daily_fluxes_test_2$pred)
  return(mae)
}
