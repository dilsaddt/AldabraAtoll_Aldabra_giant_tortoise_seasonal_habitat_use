## House keeping ----
##########################

rm(list = ls())

## Loading libraries ----
##############################

load.libraries <- function(){
  library(dplyr)
  library(ggplot2)
  library(stringr)
  library(ggview)
  library(RColorBrewer)
}

load.libraries()

###########################################################################
# 4. Predictions
###########################################################################

# load model output: L_I_T_Int_S_H.RData

load("/path_to_model_output/L_I_T_Int_S_H.RData")

#function to round the predictions 
round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[,nums] <- round(df[,nums], digits = digits)
  
  (df)
}

###########################################################################
# 4. Predictions
###########################################################################

#lambda

#length.out seasons X mcmc list
lambda<-array(NA, dim=c(4, L_I_T_Int_S_H$mcmc.info$n.samples))

#dry, dry =0, so no beta2.p
lambda[1,] <- exp(L_I_T_Int_S_H$sims.list$beta1.lambda[,1])

lambda[2,] <- exp(L_I_T_Int_S_H$sims.list$beta1.lambda[,2]) 

lambda[3,] <- exp(L_I_T_Int_S_H$sims.list$beta1.lambda[,3])

lambda[4,] <- exp(L_I_T_Int_S_H$sims.list$beta1.lambda[,4])


# then we take the mean of the mcmc list
pm.lambda <- apply(lambda, c(1), mean) 
str(pm.lambda)

# then calculate the credible intervals
CRI.lambda<-  apply(lambda, c(1), function(x) quantile(x, c(0.025, 0.975)))
str(CRI.lambda)

lambda.prob <- expand.grid(Island=c('Grande Terre East', 'Grande Terre West', 'Malabar', 'Picard'))

lambda.prob$pred[1:4] <- pm.lambda[1:4]

lambda.prob$lower[1:4] <- CRI.lambda[1,1:4]

lambda.prob$upper[1:4] <- CRI.lambda[2,1:4]

lambda.prob_round<-round_df(lambda.prob, 3)
lambda.prob_round

# Define the color palette
my_colors <- brewer.pal(3, "Set1")

# Create the plot

l.pred <- ggplot(lambda.prob, aes(Island, pred, ymin = lower, ymax = upper)) +
  geom_crossbar(width = 0.3, position = position_dodge(width = 0.8), fill = NA, color = "#808000") +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    text = element_text(size = 20),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 18),
    axis.text.y = element_text(size = 18),
    axis.title = element_text(size = 22),
    axis.title.x = element_text(margin = margin(t = 10)),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    strip.background = element_rect(fill = "white", color = "gray60"),
    strip.text = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 25, hjust = 0.5)
  ) +
  ylab(expression(paste("Expected Site-specific Density " (lambda)))) +
  xlab("Island") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), breaks = seq(0, 25, by = 5), limits = c(0, 25)) +
  labs(title = (expression(paste(lambda,'~Island'))))

# Display the plot using ggview
ggview(l.pred, width = 8, height = 6, units = "in")


# Save the plot as an image file
ggsave("L_pred.png", l.pred, width = 8, height = 6, units = "in", dpi = 300, bg = "white")

#theta

#length.out seasons X habitat X mcmc list
theta<-array(NA, dim=c(2,8, L_I_T_Int_S_H$mcmc.info$n.samples))

# dry season, beta2.theta= 

theta[1,1,] <- plogis(L_I_T_Int_S_H$sims.list$beta1.theta[,1,1])

theta[1,2,] <- plogis(L_I_T_Int_S_H$sims.list$beta1.theta[,2,1]) 

theta[1,3,] <- plogis(L_I_T_Int_S_H$sims.list$beta1.theta[,3,1])

theta[1,4,] <- plogis(L_I_T_Int_S_H$sims.list$beta1.theta[,4,1]) 

theta[1,5,] <- plogis(L_I_T_Int_S_H$sims.list$beta1.theta[,5,1])

theta[1,6,] <- plogis(L_I_T_Int_S_H$sims.list$beta1.theta[,6,1])

theta[1,7,] <- plogis(L_I_T_Int_S_H$sims.list$beta1.theta[,7,1])

theta[1,8,] <- plogis(L_I_T_Int_S_H$sims.list$beta1.theta[,8,1])

#wet, beta2.lamba =1
theta[2,1,] <- plogis(L_I_T_Int_S_H$sims.list$beta1.theta[,1,2])

theta[2,2,] <- plogis(L_I_T_Int_S_H$sims.list$beta1.theta[,2,2]) 

theta[2,3,] <- plogis(L_I_T_Int_S_H$sims.list$beta1.theta[,3,2])

theta[2,4,] <- plogis(L_I_T_Int_S_H$sims.list$beta1.theta[,4,2]) 

theta[2,5,] <- plogis(L_I_T_Int_S_H$sims.list$beta1.theta[,5,2])

theta[2,6,] <- plogis(L_I_T_Int_S_H$sims.list$beta1.theta[,6,2])

theta[2,7,] <- plogis(L_I_T_Int_S_H$sims.list$beta1.theta[,7,2])

theta[2,8,] <- plogis(L_I_T_Int_S_H$sims.list$beta1.theta[,8,2])
str(theta)
# then we take the mean of the mcmc list
pm.theta <- apply(theta, c(1,2), mean) 
str(pm.theta)

# then calculate the credible intervals
CRI.theta<-  apply(theta, c(1,2), function(x) quantile(x, c(0.025, 0.975)))
str(CRI.theta)

theta.prob <- expand.grid(Habitat=c('SMX','OMX','PEM','EXP','CAC','GRA','CHP','SND'), Season=c('Dry','Wet'))

theta.prob$pred[1:8] <- pm.theta[1,]
theta.prob$pred[9:16] <- pm.theta[2,]

theta.prob$lower[1:8] <- CRI.theta[1,1,1:8]
theta.prob$lower[9:16] <- CRI.theta[1,2,1:8]

theta.prob$upper[1:8] <- CRI.theta[2,1,1:8]
theta.prob$upper[9:16] <- CRI.theta[2,2,1:8]


theta.prob_round<-round_df(theta.prob, 3)
theta.prob_round

#create plot

label_names<-c(SMX='Standard mixed scrub',OMX='Open mixed scrub',PEM='Pemphis scrub',EXP='Exposed surface',CAC='Casuarina & coconut palms',GRA='Grassland',CHP='Champignon',SND='Sand')

theta.pred <- ggplot(data = theta.prob) +
  facet_wrap(vars(Habitat), nrow = 2, scales = "free_x", labeller = labeller(Habitat = label_names)) +
  geom_crossbar(aes(Season, pred, col = Season, ymin = lower, ymax = upper), width = 0.3, position = position_dodge(width = 0.8)) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    text = element_text(size = 20),
    axis.text.x = element_text( hjust = 0.5, size = 18),
    axis.text.y=element_text(size=18),
    axis.title= element_text(size=22),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 20),
    strip.background = element_rect(fill = "white", color = "gray60"),
    strip.text = element_text(size = 14, face = "bold"),  # Adjust the size of the label names here
    plot.title = element_text(size = 25, hjust = 0.5)
  ) +
  ylab(expression(paste("Availability " (theta)))) +
  xlab(NULL) +
  scale_x_discrete(labels = function(x) ifelse(x %in% c("Dry", "Wet"), "", x)) +
  scale_color_manual(values = my_colors) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), breaks = seq(0, 0.3, by = 0.1), limits = c(0, 0.3)) +
  labs(title = (expression(paste(theta,'~Habitat*Season'))))


ggview(theta.pred, width=12, height = 8, units='in')

ggsave("T_Pred.png", theta.pred, width = 12, height = 8, units = "in", dpi = 320, bg = "white")
