#Oi, eu sou o Jober, este código faz parte do meu portifólio em Data Science.
#Você pode conferir a minha análise completa em joberconde.medium.com
#Meu email é joberconde@gmail.com

############carregando todos os pacotes###############

library(tidyverse)
library(qgraph)
library(cowplot)
library(factoextra)
library(emmeans)
library(formattable)
library(psych)
library(DBI)
library(RSQLite)

#############obtenção e preparo dos dados###############

setwd('~/Desktop/enem') #seta o diretório de trabalho

siglaRegiao = read.delim('siglaregiao.txt') #carrega o arquivo com as siglas das UF

dados = read_delim('MICRODADOS_ENEM_2020.csv', #lê dos dados do enem
                  delim = ';',
                  escape_double = F,
                  trim_ws = T) %>%
  select(NU_INSCRICAO, #seleciona os dados
         SG_UF_ESC,
         NU_NOTA_CN,
         NU_NOTA_CH,
         NU_NOTA_LC,
         NU_NOTA_MT,
         NU_NOTA_REDACAO) %>%
  drop_na() %>% #elimina os NA
  left_join(siglaRegiao, #cria a coluna de região
            by=c('SG_UF_ESC' = 'sigla')) %>%
  mutate(regiao_uf = paste0(regiao, #cria a coluna de região_uf
                            '_',
                            SG_UF_ESC)) %>%
  rename ('Inscrição' = NU_INSCRICAO, #renomeia as colunas
          'UF' = SG_UF_ESC,
          'Ciências da natureza' = NU_NOTA_CN,
          'Ciências humanas' = NU_NOTA_CH,
          'Linguagens e códigos' = NU_NOTA_LC,
          'Matemática' =  NU_NOTA_MT,
          'Redação' = NU_NOTA_REDACAO,
          'Região' = regiao,
          'Região_UF' = regiao_uf)

provas = c('Ciências da natureza', #vetor com títulos para gráficos
           'Ciências humanas',
           'Linguagens e códigos',
           'Matemática',
           'Redação')


##################análise exploratória################

boxplots = vector('list', #cria uma lista para guardar os meus plots
                  5)


for (i in 3:7) { #cria cada um dos plots iterativamente
  boxplots[[i-2]] = ggplot(dados, #define os dados
                           aes(x = Região_UF,
                               y = dados[[i]],
                               fill = Região)) +
    xlab('UF') + #customiza o gráfico
    ylab(provas[[i-2]]) +
    geom_boxplot(show.legend = F) +
    coord_flip() +
    theme_cowplot(12) +
    theme(axis.text.x = element_text(angle = 90,
                                     hjust = 1)) +
    scale_y_continuous(breaks = seq(0,
                                    1000,
                                    100))
}

pGrid = plot_grid(boxplots[[1]], #cria um plot com vários gráficos
                  boxplots[[2]],
                  boxplots[[3]],
                  boxplots[[4]],
                  boxplots[[5]],
                  ncol = 2)

save_plot('boxplot.pdf', #salva e exporta
          pGrid,
          base_width = 10,
          base_height = 15)

###############PCA###############################

dadosPca = dados %>% #ajusta os dados
  select(c('Ciências da natureza':'Redação',
           UF)) %>%
  group_by(UF) %>% #agrupa por UF
  summarise_all(c('mean')) %>% #mostra a média
  left_join(siglaRegiao, #adiciona a coluna de região
            by=c('UF' = 'sigla')) %>%
column_to_rownames(var = 'UF')

pca = dadosPca %>%
  select('Ciências da natureza':'Redação') %>%
  prcomp(scale = T, #calcula a PCA
         center = T) %>%
  fviz_pca_biplot(habillage = dadosPca$regiao, #plota a gráfico
                  addEllipses = T,
                  ellipse.level = 0.95,
                  ellipse.type = 'convex',
                  repel = T,
                  col.var = 'black',
                  pointshape = 19) +
  theme_classic()

ggsave('pca.pdf', #exporta o gráfico
       width = 7,
       height = 3.5,
       units = "in")

####################rede de correlação####################

cormat = cor(dados[3:7]) #cria a matriz de correlação

pdf('corr.pdf', #abre o dispositivo
    width = 100,
    height = 100)

qgraph(cormat, #plota a rede
       shape = 'circle',
       posCol = 'darkgreen', 
       negCol = 'darkred',
       vsize = 10,
       minimum = 'sig',
       alpha = 0.05,
       sampleSize = 18,
       labels = provas[1:5],
       graph = 'cor')

dev.off() #fecha o dispositivo

#######################Tamanho do efeito######

efeitoTamanho = vector('list' #cria uma lista para armazenar os resultados
               , 5)

for (i in 3:7) {
  m = lm(unlist(dados[,i], #cria o modelo linear
                use.names = F) ~ factor(dados$Região))
  
  em = emmeans(m, #extrai os dados
               ~ Região)
  
  efeitoTamanho[[i-2]] = as.data.frame(eff_size(em, #calcula o valor d
                                       sigma = sigma(m),
                                       edf = df.residual(m)))
  }

df = cbind(efeitoTamanho[[1]][c(1,2)], #organiza a tabela
           efeitoTamanho[[2]][2],
           efeitoTamanho[[3]][2],
           efeitoTamanho[[4]][2],
           efeitoTamanho[[5]][2])

df = cbind(df$contrast, #arredonda e coloca os números em valores absolutos
           round(abs(df[,2:6]),
                 2))

colnames(df) = c('Contraste', #renomeia as colunas
                 'Ciências da natureza',
                 'Ciências humanas',
                 'Linguagens e códigos',
                 'Matemática',
                 'Redação')

formatar = formatter('span', #cria função de formatação
                                  style = x ~ style(font.weight = 'bold',
                                                    color = ifelse(x <= .20,
                                                                   'green',
                                                                   ifelse(x > .2 & x<= .5,
                                                                          'gold',
                                                                          'red'))))

tabela = formattable(df, #cria a tabela
                     align = c('l', #alinha as colunas
                               'c',
                               'c',
                               'c',
                               'c',
                               'c'),
                     list('Indicator Name' = formatter('span',
                                                       style = ~ style(color = 'grey',
                                                                       font.weight = "bold")),
                          'Ciências da natureza' = formatar,
                          'Ciências humanas' = formatar,
                          'Linguagens e códigos' = formatar,
                          'Matemática' = formatar,
                          'Redação' = formatar))

tabela #abre a tabela

###############################################################################

descricao = dados %>% #sumariza os dados
  select('Ciências da natureza':'Redação') %>%
  describe()

descricaoEnem = formattable(descricao, #cria e formata a tabela
                            list('Indicator Name' = formatter('span',
                                                       style = ~ style(color = 'grey',
                                                                       font.weight = 'bold'))))
descricaoEnem #abre a tabela


dadosSemZero = dados #copia os dados

dadosSemZero[dadosSemZero==0] = NA #torna os zeros em NA

dadosSemZero = drop_na(dadosSemZero) #elimina os NA

descricaoSemZero = dadosSemZero %>% #sumariza os dados sem NA
  select('Ciências da natureza':'Redação') %>%
  describe()

descricaoEnemSemZero = formattable(descricaoSemZero, #cria e formata a tabela sem os zeros
                            list('Indicator Name' = formatter('span',
                                                              style = ~ style(color = 'grey',
                                                                              font.weight = 'bold'))))
descricaoEnemSemZero #abre a tabela sem os zeros

##########SQL##############

#como bônus, trouxe esse código para guardar qualquer dataframe de uma
#maneira simples no formato SQL, basta apenas trocar os nomes das variáveis
enemdb = dbConnect(RSQLite::SQLite(),
                    'enemdb.sqlite')

dbWriteTable(enemdb,
             'enem',
             dados)

dbListTables(enemdb)

dbDisconnect(enemdb)
