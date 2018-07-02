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
dados$result[dados$result=='Victory']<-1
dados$result[dados$result=='Defeat']<-0

# Converte o tipo
dados$result<-as.integer(dados$result)

# Win rate para toda a base
tamanho = length(nomes_champ)
for (x in 1:tamanho){
  champ_posicao<-which(grepl(nomes_champ[x],dados$team_1)|grepl(nomes_champ[x],dados$team_2))
  win_rate<-as.integer(dados$result[champ_posicao])
  valor<-mean(win_rate)
  print(paste('Champion ', nomes_champ[x], ' possuiu uma win rate de ', valor, '!'))
}

champ_posicao<-which(grepl('Alistar',dados$team_1)|grepl('Alistar',dados$team_2))
win_rate<-as.integer(dados$result[champ_posicao])
mean(win_rate)
