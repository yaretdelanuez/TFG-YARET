library(cSEM)
install.packages("rootSolve")
#Directorio
setwd("X:/yaret universidad/6ºaño/TFT/codigo")

#Datos
data <- read.csv(file = "BD_cleaned_Yaret_v2.csv", header = TRUE, sep = ";")



#primer modelo estudiado
model_v1 = "

#Reflective model 
  jobSatisfaction =~ jobSatisfaction_1 + jobSatisfaction_2 + jobSatisfaction_3 + jobSatisfaction_4 + jobSatisfaction_5 + jobSatisfaction_6
  orgSupport =~ orgSupport_1 + orgSupport_2 + orgSupport_3 + orgSupport_4 + orgSupport_5 + orgSupport_6 + orgSupport_7 + orgSupport_8
  performance =~ performance_1 + performance_2 + performance_3 + performance_4

#Structural model
jobSatisfaction ~ orgSupport
performance ~ orgSupport + jobSatisfaction  + orgSupport.jobSatisfaction


"

# Perform estimation
out <- csem(.data = data, .model = model_v1,
            # To reproduce the ADANCO results
            .PLS_weight_scheme_inner = 'factorial',
            .tolerance = 1e-06,
            .resample_method = 'bootstrap'
)


#Resumen
summarize(out)
summarize <- summarize(out)

#loadings > 0.7
out$Estimates$Loading_estimates
#write.csv(x = out$Estimates$Loading_estimates, file = "Loading_estimates.csv")


#Despues de analizar los loadings quitamos las variables que no cumplen los requisitos
#eliminado jobSatisfaction_2 y performance_4  (y + orgSupport.jobSatisfaction para poder calcular los valores del modelo)   
#Por eso creamos otro modelo
model_v2 = "

#Reflective model 
  jobSatisfaction =~  jobSatisfaction_1 + jobSatisfaction_3 + jobSatisfaction_4  + jobSatisfaction_5 + jobSatisfaction_6
  orgSupport =~ orgSupport_1 + orgSupport_2 + orgSupport_3 + orgSupport_4 + orgSupport_5 + orgSupport_6 + orgSupport_7 + orgSupport_8
  performance =~ performance_1 + performance_2 + performance_3 

#Structural model
jobSatisfaction ~ orgSupport
performance ~ orgSupport + jobSatisfaction  #+ orgSupport.jobSatisfaction
"

#Estimamos
out <- csem(.data = data, .model = model_v2,
            # To reproduce the ADANCO results
            .PLS_weight_scheme_inner = 'factorial',
            .tolerance = 1e-06,
            .resample_method = 'bootstrap'
)


#Resumen
summarize(out)
summarize <- summarize(out)

#loadings > 0.7
out$Estimates$Loading_estimates

#Evaluar
assess(out)
assess <- assess(out)
#de assess sacamos alpha de cronbachs, rhoC, rhoA, AVE

#sacar datos fornell-larcker
assess$`Fornell-Larcker`
#write.csv(x = assess$`Fornell-Larcker`, file = "Fornell-Larcker.csv")
#htmt
assess$HTMT$htmts
#sacar datos de HTMT
#write.csv(x = assess$HTMT$htmts, file = "HTMT.csv")
#VIF
assess$VIF
#sacar datos de VIF
#write.csv(x = assess$VIF, file = "VIF.csv")
#rmsea
assess$RMSEA
#srmr
assess$SRMR
assess$Effects$Direct_effect
#Efecto indirecto
assess$Effects$Indirect_effect
#sacar datos efecto indirecto
#write.csv(x = assess$Effects$Indirect_effect, file = "efectoIndirecto.csv")
assess$Effects$Total_effect
#write.csv(x = assess$Effects$Total_effect, file = "efectoTotal.csv")

#con la moderadora y sin performance_4 y jobSatisfaction_2 para calcular los datos restantes
model_v2_final = "

#Reflective model 
  jobSatisfaction =~  jobSatisfaction_1 + jobSatisfaction_3 + jobSatisfaction_4  + jobSatisfaction_5 + jobSatisfaction_6
  orgSupport =~ orgSupport_1 + orgSupport_2 + orgSupport_3 + orgSupport_4 + orgSupport_5 + orgSupport_6 + orgSupport_7 + orgSupport_8
  performance =~ performance_1 + performance_2 + performance_3 

#Structural model
jobSatisfaction ~ orgSupport
performance ~ orgSupport + jobSatisfaction  + orgSupport.jobSatisfaction
"

out2 <- csem(.data = data, .model = model_v2_final,
             # To reproduce the ADANCO results
             .PLS_weight_scheme_inner = 'factorial',
             .tolerance = 1e-06,
             .resample_method = 'bootstrap'
)


# Return summary
summarize(out2)
summarize2 <- summarize(out2)
out2$Estimates$Path_estimates
#estimaciones 
summarize2$Estimates$Path_estimates
#write.csv(x = summarize2$Estimates$Path_estimate, file = "path_estimate.csv")

out2$Estimates$R2
#write.csv(x = out2$Estimates$R2, file = "R2.csv")
out2$Estimates$R2adj
#write.csv(x = out2$Estimates$R2adj, file = "R2adj.csv")



#moderadora
# Realiza análisis de efectos no lineales con la función doNonlinearEffectsAnalysis() 
neffects <- doNonlinearEffectsAnalysis(out2, 
                                       .dependent = 'performance',
                                       .moderator = 'jobSatisfaction',
                                       .independent = 'orgSupport') 
# Efectos simples gráfico
plot(neffects, .plot_type = 'simpleeffects')




#preparando datos de constructos para nca, con el out2 que es el model final
construct<-getConstructScores(out2)

construct$Construct_scores
#exportamos los datos para nca y redes
#write.csv(x = construct$Construct_scores, file = "variables_latentes.csv")


#predicción
predict(.handle_inadmissibles = 'ignore', out)

