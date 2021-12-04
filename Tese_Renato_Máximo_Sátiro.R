# Universidade Federal de Goiás - UFG
# Faculdade de Administração, Ciências Contábeis e Ciências Econômicas - FACE/UFG
# Programa de Pós Graduação em Administração - PPGADM/UFG
# Doutorado em Administração
# Renato Máximo Sátiro

##################################################################################
#                 INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS               #
##################################################################################
#Pacotes utilizados
pacotes <- c("plotly","tidyverse","ggrepel","fastDummies","knitr","kableExtra",
             "splines","reshape2","PerformanceAnalytics","metan","correlation",
             "see","ggraph","nortest","rgl","car","olsrr","jtools","ggstance",
             "magick","cowplot","beepr","Rcpp")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

Sys.setlocale("LC_ALL", "pt_BR.UTF-8")

options(encoding = "UTF8")

##################################################################################
#                               REGRESSÃO LINEAR                                 #
##################################################################################
#Carregando a base de dados
library(readxl)
base <- read_excel("C:/Users/casas bahia/Desktop/Doutorado/Artigos/Acesso Geral/IAJ_Base_Tribunais.xls")

hist(base$Casos_novos_por_100kh_1GJETR, main = "Histograma Casos novos / 100 mil habitantes", col = "green")

options(scipen = 1000)

#Estatísticas univariadas
summary(base, digits = 2)

##################################################################################
#                               ESTUDO DAS CORRELAÇÕES                           #
##################################################################################
#A função correlation do pacote correlation faz com que seja estruturado um
#diagrama interessante que mostra a inter-relaçãoo entre as variáveis e a
#magnitude das correlações entre elas
#Requer instação e carregamento dos pacotes see e ggraph para a plotagem
library(readxl)
base2 <- read_excel("C:/Users/casas bahia/Desktop/Doutorado/Artigos/Acesso Geral/base2.xlsx")
View(base2)

base2 %>%
  correlation(method = "pearson") %>%
  plot()

#A função chart.Correlation do pacote PerformanceAnalytics apresenta as
#distribuições das variáveis, scatters, valores das correlações e suas
#respectivas significâncias
chart.Correlation(base2, histogram = TRUE)

#A funçãoo corr_plot do pacote metan também apresenta as distribuições
#das variéveis, scatters, valores das correlações e suas respectivas
#significâncias
base2 %>%
  corr_plot(shape.point = 21,
            col.point = "black",
            fill.point = "#FDE725FF",
            size.point = 2,
            alpha.point = 0.6,
            maxsize = 4,
            minsize = 2,
            smooth = TRUE,
            col.smooth = "black",
            col.sign = "#440154FF",
            upper = "corr",
            lower = "scatter",
            diag.type = "density",
            col.diag = "#440154FF",
            pan.spacing = 0,
            lab.position = "bl")

##################################################################################
#                          REGRESSÃO LINEAR MULTIPLA                             #
#                        CARREGAMENTO DA BASE DE DADOS                           #
##################################################################################
#EstimaÃ§Ã£o do modelo OLS linear
modelo_linear <- lm(formula = base$Casos_novos_por_100kh_1GJETR ~ base$dinf1_mag + base$dinf2_mag + base$f4a+
                      base$tfauxt+base$h2+base$pib+base$gt+base$Taxa_agua_canalizada+
                      base$Taxa_nasc_vivos_maes10a19+base$Analfabetismo_maiorde15+base$DAI+base$DPI+
                      base$Coef_Gini+base$Taxa_escolarizacao+base$Taxa_mulher+base$Idade_50emais+base$Cor_Raca_Nao_Branca+base$Tempo_medio_decisao+
                      +base$Taxa_varas_por_pop+base$Taxa_mag_por_pop+base$k,
                    data = base)

summary(modelo_linear)

modelo_step_linear <- step(lm(formula = base$Casos_novos_por_100kh_1GJETR ~ base$dinf1_mag + base$dinf2_mag + base$f4a+
                      base$tfauxt+base$h2+base$pib+base$gt+base$Taxa_agua_canalizada+
                      base$Taxa_nasc_vivos_maes10a19+base$Analfabetismo_maiorde15+base$DAI+base$DPI+
                      base$Coef_Gini+base$Taxa_escolarizacao+base$Taxa_mulher+base$Idade_50emais+base$Cor_Raca_Nao_Branca+base$Tempo_medio_decisao+
                      +base$Taxa_varas_por_pop+base$Taxa_mag_por_pop+base$k,
                    data = base))

summary(modelo_step_linear)


library(stargazer)
stargazer(modelo_linear, modelo_step_linear, type = "text", title = "Resultados da Regressão Linear Múltipla", dep.var.labels = "Casos novos por 100 mil habitantes", digits = 2, out = "Modelo.html")

#Outras maneiras de apresentar os outputs do modelo
#função summ do pacote jtools
summ(modelo_linear, confint = F, digits = 2, ci.width = .95)
export_summs(modelo_linear, scale = F, digits = 4)

##################################################################################
#          TESTE DE VERIFICAÇÃO DA ADERÊCIA DOS RESÍDUOS À NORMALIDADE           #
#                               SHAPIRO-FRANCIA                                  #
##################################################################################
#Shapiro-Wilk: n <= 30
## shapiro.test(modelo_linear$residuals)

#Shapiro-Francia: n > 30
sf.test(modelo_linear$residuals) #funÃ§Ã£o sf.test do pacote nortest

sf.test(modelo_step_linear$residuals)

#Histograma dos resíduos do modelo OLS linear
base %>%
  mutate(residuos = modelo_linear$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "grey50", 
                 fill = "grey90", 
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(modelo_linear$residuals),
                            sd = sd(modelo_linear$residuals)),
                aes(color = "Curva Normal Teórica"),
                size = 2) +
  scale_color_manual("Legenda:",
                     values = "#FDE725FF") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")

#Ajustes dos modelos: valores previstos (fitted values) X valores reais
base %>%
  ggplot() +
  geom_smooth(aes(x = base$target.tbaix, y = yhat_linear, color = "OLS Linear"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5), size = 1.5) +
  geom_point(aes(x = base$target.tbaix, y = yhat_linear),
             color = "#FDE725FF", alpha = 0.6, size = 2) +
  geom_smooth(aes(x = base$target.tbaix, y = yhat_modelo_bc, color = "Box-Cox"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5), size = 1.5) +
  geom_point(aes(x = base$target.tbaix, y = yhat_modelo_bc),
             color = "#440154FF", alpha = 0.6, size = 2) +
  geom_smooth(aes(x = base$target.tbaix, y = base$target.tbaix), method = "lm", 
              color = "gray30", size = 1.05,
              linetype = "longdash") +
  scale_color_manual("Modelos:", 
                     values = c("#440154FF", "#FDE725FF")) +
  labs(x = "Processos", y = "Fitted Values") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")

#Plotando os resíuos do modelo
base %>%
  mutate(residuos = modelo_bc$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(color = "white", 
                 fill = "#440154FF", 
                 bins = 30,
                 alpha = 0.6) +
  labs(x = "Resíduos",
       y = "Frequência") + 
  theme_bw()

#Acrescentando uma curva normal teórica para compação entre as distribuições
base %>%
  mutate(residuos = modelo_bc$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "white", 
                 fill = "#440154FF", 
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(modelo_bc$residuals),
                            sd = sd(modelo_bc$residuals)),
                size = 2, color = "grey30") +
  scale_color_manual(values = "grey50") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme_bw()

##################################################################################
#            DIAGNÓSTICO DE MULTICOLINEARIDADE EM MODELOS DE REGRESSÃO           #
##################################################################################
#Diagnóstico de multicolinearidade (Variance Inflation Factor e Tolerance)
ols_vif_tol(modelo_linear)
#função ols_vif_tol do pacote olsrr

##################################################################################
#           DIAGNÓSTICO DE HETEROCEDASTICIDADE EM MODELOS DE REGRESSÃO           #
##################################################################################
#Teste de Breusch-Pagan para diagnóstico de heterocedasticidade
ols_test_breusch_pagan(modelo_linear)
#função ols_test_breusch_pagan do pacote olsrr
#Presença de heterocedasticidade -> omissão de variável(is) explicativa(s) relevante(s)

#H0 do teste: ausência de heterocedasticidade.
#H1 do teste: heterocedasticidade, ou seja, correlação entre resíduos e uma ou mais
#variáveis explicativas, o que indica omissão de variável relevante!

durbinWatsonTest(modelo_linear)

####################################### FIM #######################################