library(ggplot2)
work_station<-'C:/Users/ricardo/Documents/CAD'
setwd(work_station)

library(tidyr)
champ_names<-c( 'Aatrox','Ahri','Akali','Alistar','Amumu','Anivia','Annie','Ashe',
                'Aurelion Sol', 'Azir', 'Bard','Blitzcrank','Brand','Braum','Caitlyn','Camille',
                'Cassiopeia',"Cho'Gath",'Corki','Darius','Diana','Dr. Mundo','Draven','Ekko',
                'Elise','Evelynn','Ezreal','Fiddlesticks','Fiora','Fizz','Galio','Gangplank',
                'Garen','Gnar','Gragas','Graves','Hecarim','Heimerdinger','Illaoi','Irelia',
                'Ivern','Janna','Jarvan IV','Jax','Jayce','Jhin','Jinx',"Kai'Sa",'Kalista',
                'Karma','Karthus','Kassadin','Katarina','Kayle','Kayn','Kennen',"Kha'Zix",
                'Kindred','Kled',"Kog'Maw",'LeBlanc','Lee Sin','Leona','Lissandra','Lucian',
                'Lulu','Lux','Malphite','Malzahar','Maokai','Master Yi','Miss Fortune',
                'Mordekaiser','Morgana','Nami','Nasus','Nautilus','Nidalee','Nocturne','Nunu',
                'Olaf','Orianna','Ornn','Pantheon','Poppy','Pyke','Quinn','Rakan','Rammus',
                "Rek'Sai",'Renekton','Rengar','Riven','Rumble','Ryze','Sejuani','Shaco',
                'Shen','Shyvana','Singed','Sion','Sivir','Skarner','Sona','Soraka','Swain',
                'Syndra','Tahm Kench','Taliyah','Talon','Taric','Teemo','Thresh','Tristana',
                'Trundle','Tryndamere','Twisted Fate','Twitch','Udyr','Urgot','Varus',
                'Vayne','Veigar',"Vel'Koz",'Vi','Viktor','Vladimir','Volibear','Warwick',
                'Wukong','Xayah','Xerath','Xin Zhao','Yasuo','Yorick','Zac','Zed','Ziggs',
                'Zilean','Zoe','Zyra')


dados<-read.csv('patch811.csv', stringsAsFactors = F)

dados_old <- read.csv('patch811.csv', stringsAsFactors = F)
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

data_champion<-data.frame(nomes=win_rate_final[,2],rate=as.numeric(win_rate_final[,1]), cor=win_rate_final[,3])

ggplot(data_champion, aes(x=c(1:length(champ_names)), y=rate)) +
  geom_point() + 
  geom_text(label=data_champion$nomes)
