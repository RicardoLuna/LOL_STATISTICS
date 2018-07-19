################################################
# Universidade Federal Rural de Pernambuco     #
# Disciplina: Computação para Análise de Dados #
# Professor: Ermerson Andrade                  #
# Alunos: Iverson Pereira                      #
#         Ricardo Luna                         #
################################################


# use this if you need to install the packages
install.packages('ggplot2')
install.packages('tidyr')
install.packages('dplyr')

# put here the libraries
library(ggplot2)
library(tidyr)
library(dplyr)

# set here your work station
work_station<-'/home/iverson/git/LOL_STATISTICS/'
setwd(work_station)

# read the file with champ names
champ_names <- read.table("champions.txt", sep=",")
champ_names <- gather(champ_names, "champ")
champ_names <- champ_names$value

dados<-read.csv('patch811.csv', stringsAsFactors = F)

# Concatena os valores dos nomes do champ como um único como uma única lista
dados<-dados%>%
  unite(col = "team_1",
        1,2,3,4,5,
        sep = "_")

dados<-dados%>%
  unite(col = "team_2",
        2,3,4,5,6,
        sep = "_")

# Converter os valores para 1 ou 0
dados$result[dados$result=='Victory']<-T
dados$result[dados$result=='Defeat']<-F

# Converte o tipo
dados$result<-as.logical(dados$result)

# Win rate para toda a base
tamanho = length(champ_names)
win_rate_final<-matrix(1:3*length(champ_names),length(champ_names),3)
for (x in 1:tamanho){
  champ_posicao<-which(grepl(champ_names[x],dados$team_1))
  champ_posicao2<-which(grepl(champ_names[x],dados$team_2))
  win_rate<-dados$result[champ_posicao]
  win_temp<-!dados$result[champ_posicao2]
  win_rate<-c(win_rate,win_temp)
  valor<-mean(win_rate)
  print(paste('Champion ', champ_names[x], ' possuiu uma win rate de ', valor, '!'))
  win_rate_final[x,1]<-valor
  win_rate_final[x,2]<-champ_names[x]
  win_rate_final[x,3]<-colors()[x]
}

data_champion<-data.frame(nomes=win_rate_final[,2],
                          rate=as.numeric(win_rate_final[,1]),
                          cor=win_rate_final[,3])

ggplot(data_champion, aes(x=c(1:length(champ_names)), y=rate)) +
  geom_point() + 
  geom_text(label=data_champion$nomes)

# champiom's win rate by region
rate_by_region <- c()
regions <- unique(dados$server)
tamanho = length(champ_names)
for (x in 1:tamanho){
  champ_pos_team1 <- which(grepl(champ_names[x],dados$team_1))
  champ_pos_team2 <- which(grepl(champ_names[x],dados$team_2))
  champ_data_team1 <- dados[champ_pos_team1, ]
  champ_data_team2 <- dados[champ_pos_team2, ]
  champ_data_team2$result <- !champ_data_team2$result
  
  champ_data <- rbind(champ_data_team1, champ_data_team2)
  
  champ_rate_by_region <- champ_data %>%
                    group_by(server) %>%
                    summarise(
                      rate = mean(result)
                    )
  
  champ_popularity <- length(champ_data$server)
  
  rate_by_region <- rbind(rate_by_region, data.frame(champ=champ_names[x], champ_rate_by_region, popularity = champ_popularity))
}

# victory rate per region independ of the champion
higher_rates <- rate_by_region %>%
                group_by(champ,server) %>%
                summarise(rate = max(rate))

# most popular champions in the world
most_popular <- rate_by_region %>%
  group_by(champ) %>%
  summarise(count = max(popularity)) %>%
  arrange(desc(count))

colors <- c("green", "red", "blue", "yellow", "pink", "orange")

barplot(head(most_popular$count), 
        names.arg=head(most_popular$champ), 
        main="Most popular champions - Global",
        xlab="Champion",
        ylab="Matches",
        col=colors)

# TODO - generate most popular champions per region

# get the global rate for each champion
global_rate <- aggregate(formula = rate ~ champ,
                        FUN = mean,
                        data=rate_by_region)

