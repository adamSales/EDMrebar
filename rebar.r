library(sandwich)
library(ggplot2)

dat <- read.csv('updated_exp_predictions.csv')
dat <- subset(dat,ExperiencedCondition)
dat$problem_set <- as.factor(dat$problem_set)

### analysis for "complete"
dat <- droplevels(dat[!is.na(dat$p_complete),])

### sample sizes?
table(dat$problem_set,dat$condition)


### how good are the predictions?
dat$completeR <- dat$complete-dat$p_complete

(summary(m1 <- lm(complete~problem_set,data=dat,subset=condition=='C')))
(summary(m1 <- lm(completeR~problem_set,data=dat,subset=condition=='C')))

res <- NULL
for(ps in unique(dat$problem_set)){
    mod1 <- lm(complete~condition,subset=problem_set==ps,data=dat)
    se1 <- sqrt(vcovHC(mod1,'HC2')[2,2])
    mod2 <- lm(completeR~condition,subset=problem_set==ps,data=dat)
    se2 <- sqrt(vcovHC(mod2,'HC2')[2,2])
    res <- rbind(res,c(ps=as.numeric(ps),regEst=coef(mod1)[2],regSE=se1,rebarEst=coef(mod2)[2],rebarSE=se2))
}

#qplot(res[,'regSE'],res[,'rebarSE'],xlim=range(c(res[,'regSE'],res[,'rebarSE'])),ylim=range(c(res[,'regSE'],res[,'rebarSE'])),xlab='Unadjusted Standard Errors',ylab='Rebar Standard Errors')+geom_abline(intercept=0,slope=1,linetype='dotted')+coord_fixed()

#qplot(1:22,sort((res[,'rebarSE']-res[,'regSE'])/res[,'regSE'])*100,ylab='% Change in SE')+geom_hline(yintercept=0,linetype='dotted')+scale_x_continuous(NULL)+scale_y_continuous(breaks=seq(-40,10,10),labels=paste0(seq(-40,10,10),'%'))


plotDat <- as.data.frame(rbind(res[order(res[,2]),2:3],res[order(res[,2]),c(4,5)])*100)
plotDat$type=factor(rep(c('Usual','Rebar'),each=nrow(res)),levels=c('Usual','Rebar'))
plotDat$x <- c(1:22,1:22+0.2)
names(plotDat)=c('est','se','type','x')

ggplot(plotDat,aes(x,est,fill=type,color=type))+geom_point()+geom_errorbar(aes(ymin=est-2*se,ymax=est+2*se))+geom_hline(yintercept=0,linetype='dotted')+ylab('Estimated Treatment Effect\n(Percentage Point)')+xlab('Experiment')+#scale_x_continuous(breaks=seq(1.1,22.1,1),labels=1:22)+
    labs(color=NULL,fill=NULL)+
         theme(legend.position='bottom',text=element_text(size=7.5),
              axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank())#theme(legend.position='top',text=element_text(size=7.5))
ggsave('estEff1.jpg',width=3,height=3)

cors <- sapply(unique(dat$problem_set),function(ps) with(subset(dat,problem_set==ps),cor(p_complete,complete)))
predR2 <- sapply(unique(dat$problem_set),function(ps) with(subset(dat,problem_set==ps),1-sum((p_complete-complete)^2)/sum((complete-mean(complete))^2)))
mses <- sapply(unique(dat$problem_set),function(ps) with(subset(dat,problem_set==ps),mean((p_complete-complete)^2)))
seDiff <- (res[,'regSE']-res[,'rebarSE'])/res[,'regSE']
qplot(predR2,seDiff*100,xlab=expression(paste('Prediction ',R^2)),ylab='% SE Reduction')+geom_hline(yintercept=0,linetype='dotted')+scale_y_continuous(breaks=seq(-10,50,10),labels=paste0(seq(-10,50,10),'%'))+theme(text=element_text(size=7.5))
ggsave('corVsSE.jpg',width=3,height=3)


res2 <- NULL
for(ps in unique(dat$problem_set)){
    summary(mod1 <- lm(complete~condition+Prior.Percent.Correct+Prior.Percent.Completion,subset=problem_set==ps,data=dat))
    se1 <- sqrt(vcovHC(mod1,'HC2')[2,2])
    mod2 <- lm(completeR~condition+Prior.Percent.Correct+Prior.Percent.Completion,subset=problem_set==ps,data=dat)
    se2 <- sqrt(vcovHC(mod2,'HC2')[2,2])
    res2 <- rbind(res2,c(ps=as.numeric(ps),regEst=coef(mod1)[2],regSE=se1,rebarEst=coef(mod2)[2],rebarSE=se2))
}

plotDat <- as.data.frame(rbind(res2[order(res2[,2]),2:3],res2[order(res2[,2]),c(4,5)])*100)
plotDat$type=factor(rep(c('Usual','Rebar'),each=nrow(res2)),levels=c('Usual','Rebar'))
plotDat$x <- c(1:22,1:22+0.2)
names(plotDat)=c('est','se','type','x')


ggplot(plotDat,aes(x,est,fill=type,color=type))+geom_point()+geom_errorbar(aes(ymin=est-2*se,ymax=est+2*se))+geom_hline(yintercept=0,linetype='dotted')+ylab('Estimated Treatment Effect\n(Percentage Point)')+xlab('Experiment')+#+scale_x_continuous(NULL) +#breaks=seq(1.1,22.1,1),labels=1:22)+
    labs(color=NULL,fill=NULL)+
        theme(legend.position='top',#text=element_text(size=7.5),
              axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank())
ggsave('estEff2.jpg')#,width=3,height=3)

xcors <- sapply(unique(dat$problem_set),function(ps) with(subset(dat,problem_set==ps),cor(p_complete,complete)))
seDiff <- (res2[,'regSE']-res2[,'rebarSE'])/res2[,'regSE']
qplot(cors,seDiff*100,xlab=expression(paste('Cor(',hat(Y),',',Y,')')),ylab='% Improvement')+geom_hline(yintercept=0,linetype='dotted')+scale_y_continuous(breaks=seq(-10,50,10),labels=paste0(seq(-10,50,10),'%'))+theme(text=element_text(size=7.5))
ggsave('corVsSE2.jpg',width=3,height=3)


