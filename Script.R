dados<-read.csv("total.csv",header=T,dec=",",encoding = "UTF-8")

i=1
dados1<-dados[seq(1,17,1),]
attach(dados1)
str(dados1)
ano<-1:length(cv)
modelo <- lm(cv~ano);beta[i]<-summary(modelo)[[4]][2];pval[i]<-summary(modelo)[[4]][8];r2[i]<-summary(modelo)[[9]];nome[i]<-paste(cur[1],"-",per[1],"-",cid[1]);i=i+1

texto <- sprintf('pval = %.3f',summary(modelo)[[4]][8])

ggplot(dados1, aes(x=ano, y=cv))+
	geom_point(colour="red")+
	labs(x="Ano", y="Relação Candidato/Vaga", title=paste(cur[1],"-",per[1],"-",cid[1]))+
	theme(plot.title = element_text(hjust = 0.5))+
      geom_smooth(method=lm, se=TRUE)+
      geom_text(aes(x=min(ano), y=max(cv), label=texto), hjust=0, vjust=1);Sys.sleep(1.5)