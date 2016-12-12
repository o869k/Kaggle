#Initialization
{
    sessionInfo() #system performance
    gc() #clear unsued memory
    rm(list=ls()) # clear workspace
    setwd(mainDir)
    Sys.setlocale("LC_TIME", "English")
    set.seed(1234) #setting seed for comparison
    library(seqHMM)
    library(HMMpa);
}

#### A HMM model - Data exploration ####
data(biofam3c)

# Building sequence objects (starting at age 15)
marr.seq <- seqdef(biofam3c$married, start = 15)
child.seq <- seqdef(biofam3c$children, start = 15)
left.seq <- seqdef(biofam3c$left, start = 15)

# Choosing colours for states
attr(marr.seq, "cpal") <- c("#AB82FF", "#E6AB02", "#E7298A")
attr(child.seq, "cpal") <- c("#66C2A5", "#FC8D62")
attr(left.seq, "cpal") <- c("#A6CEE3", "#E31A1C")

# Plotting state distribution plots of observations
ssplot(list(marr.seq, child.seq, left.seq), title = "State distribution plots")

# Preparing plots for women's state distributions
# Sorting by scores from multidimensional scaling
ssp_f2 <- ssp(list(marr.seq[biofam3c$covariates$sex == "woman",], 
                   child.seq[biofam3c$covariates$sex == "woman",],
                   left.seq[biofam3c$covariates$sex == "woman",]),
              type = "d", plots = "obs", border = NA, withlegend = FALSE,
              title = "State distributions for women", title.n = FALSE,
              ylab = c("Married", "Parenthood", "Left home"), ylab.pos = c(1, 2, 1),
              xlab = "Age", xtlab = 15:30)

# Same plot, but sequences instead of state distributions
ssp_f3 <- update(ssp_f2, type = "I", sortv = "mds.obs", title = "Sequences for women")

# State distributions with men's data
ssp_m2 <- update(ssp_f2, x = list(marr.seq[biofam3c$covariates$sex == "man",], 
                                  child.seq[biofam3c$covariates$sex == "man",], 
                                  left.seq[biofam3c$covariates$sex == "man",]),
                 title = "State distributions for men")

# Men's sequences
ssp_m3 <- update(ssp_m2, type = "I", sortv = "mds.obs", title = "Sequences for men")

# Plotting state distributions and index plots of observations for women and men 
gridplot(list(ssp_f2, ssp_f3, ssp_m2, ssp_m3), ncol = 2, byrow = TRUE, 
         row.prop = c(0.42, 0.42, 0.16), legend.pos2 = "top")

#### Fit HMM Model ####
# Initial values for emission matrices
emiss_marr <- matrix(NA, nrow=4, ncol=3)
emiss_marr[1,] <- seqstatf(marr.seq[, 1:4])[, 2] + 0.1
emiss_marr[2,] <- seqstatf(marr.seq[, 5:8])[, 2] + 0.1
emiss_marr[3,] <- seqstatf(marr.seq[, 9:12])[, 2] + 0.1
emiss_marr[4,] <- seqstatf(marr.seq[, 13:16])[, 2] + 0.1
emiss_marr <- emiss_marr / rowSums(emiss_marr)

emiss_child <- matrix(NA, nrow=4, ncol=2)
emiss_child[1,] <- seqstatf(child.seq[, 1:4])[, 2] + 0.1
emiss_child[2,] <- seqstatf(child.seq[, 5:8])[, 2] + 0.1
emiss_child[3,] <- seqstatf(child.seq[, 9:12])[, 2] + 0.1
emiss_child[4,] <- seqstatf(child.seq[, 13:16])[, 2] + 0.1
emiss_child <- emiss_child / rowSums(emiss_child)

emiss_left <- matrix(NA, nrow=4, ncol=2)
emiss_left[1,] <- seqstatf(left.seq[, 1:4])[, 2] + 0.1
emiss_left[2,] <- seqstatf(left.seq[, 5:8])[, 2] + 0.1
emiss_left[3,] <- seqstatf(left.seq[, 9:12])[, 2] + 0.1
emiss_left[4,] <- seqstatf(left.seq[, 13:16])[, 2] + 0.1
emiss_left <- emiss_left / rowSums(emiss_left)

# Initial values for transition matrix
trans <- matrix(
    c(0.90, 0.06, 0.03, 0.01,
      0, 0.90, 0.07, 0.03,
      0,    0, 0.90, 0.10,
      0,    0,    0,    1), 
    nrow = 4, ncol = 4, byrow = TRUE)

# Initial values for initial state probabilities
initial_probs <- c(0.9, 0.07, 0.02, 0.01)

# Building the hidden Markov model with initial parameter values 
bhmm <- build_hmm(
    observations = list(marr.seq, child.seq, left.seq),
    initial_probs = initial_probs, transition_probs = trans, 
    emission_probs = list(emiss_marr, emiss_child, emiss_left),
    channel_names = c("Marriage", "Parenthood", "Residence"))

# Fitting the HMM:
# step 1) EM algorithm
hmm <- fit_model(bhmm)
hmm$logLik
# -16854.16

# EM + 50 restarts with random starting values for emission probabilities
hmm2 <- fit_model(bhmm, 
                  control_em = list(restart = list(times = 50, transition = FALSE, emission = TRUE)))
hmm2$logLik
# -16854.16

# Using all three steps:
# step 1) EM algorithm
# step 2) global optimization (default: MLSL_LDS with LBFGS as local optimizer)
# step 3) local optimization (default: LBFGS) for "final polishing"
# Note: By default, estimation time limited to 60 seconds in step 2.
# Setting 3000 evaluations with unlimited time
hmm3 <- fit_model(bhmm, global_step = TRUE, local_step = TRUE, control_global = list(maxeval = 3000, maxtime = 0))
hmm3$logLik
# -16854.16

# Only global optimization (3000 iterations, unlimited time)
hmm4 <- fit_model(bhmm, em_step = FALSE, global_step = TRUE, local_step = FALSE,
                  control_global = list(maxeval = 3000, maxtime = 0))
hmm4$logLik
# -16856.78

#### Plot HMM ####
plot(hmm$model)

# Plotting observations and hidden states
ssplot(hmm$model, plots = "both", type = "I")

# Likelihood
logLik(hmm$model)
# 'log Lik.' -16854.16 (df=25)

# BIC
BIC(hmm$model)
# 33967.66

#### Mixture hidden Markov models ####
# Starting values for emission probabilities

# Cluster 1
alphabet(child.seq) # Checking for the order of observed states
emiss_1_child <- matrix(
    c(0.99, 0.01, # High probability for childless
      0.99, 0.01,
      0.99, 0.01,
      0.99, 0.01), 
    nrow = 4, ncol = 2, byrow = TRUE)

alphabet(marr.seq)
emiss_1_marr <- matrix(
    c(0.01, 0.01, 0.98, # High probability for single
      0.01, 0.01, 0.98,
      0.01, 0.98, 0.01, # High probability for married
      0.98, 0.01, 0.01), # High probability for divorced
    nrow = 4, ncol = 3, byrow = TRUE)

alphabet(left.seq)
emiss_1_left <- matrix(
    c(0.01, 0.99, # High probability for living with parents
      0.99, 0.01, # High probability for having left home
      0.99, 0.01,
      0.99, 0.01), 
    nrow = 4, ncol = 2, byrow = TRUE)

# Cluster 2
emiss_2_child <- matrix(
    c(0.99, 0.01, # High probability for childless
      0.99, 0.01,
      0.99, 0.01,
      0.01, 0.99), 
    nrow = 4, ncol = 2, byrow = TRUE)

emiss_2_marr <- matrix(
    c(0.01, 0.01, 0.98, # High probability for single
      0.01, 0.01, 0.98,
      0.01, 0.98, 0.01, # High probability for married
      0.29, 0.7, 0.01), 
    nrow = 4, ncol = 3, byrow = TRUE)

emiss_2_left <- matrix(
    c(0.01, 0.99, # High probability for living with parents
      0.99, 0.01,
      0.99, 0.01,
      0.99, 0.01), 
    nrow = 4, ncol = 2, byrow = TRUE)

# Cluster 3
emiss_3_child <- matrix(
    c(0.99, 0.01, # High probability for childless
      0.99, 0.01,
      0.01, 0.99,
      0.99, 0.01,
      0.01, 0.99,
      0.01, 0.99), 
    nrow = 6, ncol = 2, byrow = TRUE)

emiss_3_marr <- matrix(
    c(0.01, 0.01, 0.98, # High probability for single
      0.01, 0.01, 0.98,
      0.01, 0.01, 0.98,
      0.01, 0.98, 0.01, # High probability for married
      0.01, 0.98, 0.01,
      0.98, 0.01, 0.01), # High probability for divorced
    nrow = 6, ncol = 3, byrow = TRUE)

emiss_3_left <- matrix(
    c(0.01, 0.99, # High probability for living with parents
      0.99, 0.01,
      0.50, 0.50,
      0.01, 0.99,
      0.99, 0.01,
      0.99, 0.01), 
    nrow = 6, ncol = 2, byrow = TRUE)

# Starting values for transition matrices
trans_1 <- matrix(
    c(0.80, 0.16, 0.03, 0.01,
      0, 0.90, 0.07, 0.03,
      0,    0, 0.90, 0.10,
      0,    0,    0,    1),
    nrow = 4, ncol = 4, byrow = TRUE)

trans_2 <- matrix(
    c(0.80, 0.10, 0.05, 0.03, 0.01, 0.01,
      0, 0.70, 0.10, 0.10, 0.05, 0.05,
      0,    0, 0.85, 0.01, 0.10, 0.04,
      0,    0,    0, 0.90, 0.05, 0.05,
      0,    0,    0,    0, 0.90,  0.1,
      0,    0,    0,    0,    0,    1),
    nrow = 6, ncol = 6, byrow = TRUE)

# Starting values for initial state probabilities
initial_probs_1 <- c(0.9, 0.07, 0.02, 0.01)
initial_probs_2 <- c(0.9, 0.04, 0.03, 0.01, 0.01, 0.01)

# Birth cohort
biofam3c$covariates$cohort <- cut(
    biofam3c$covariates$birthyr, c(1908, 1935, 1945, 1957))
biofam3c$covariates$cohort <- factor(
    biofam3c$covariates$cohort, labels=c("1909-1935", "1936-1945", "1946-1957")
)

# Build MHMM
init_mhmm <- build_mhmm(
    observations = list(marr.seq, child.seq, left.seq),
    transition_probs = list(trans_1, trans_1, trans_2),
    emission_probs = list(list(emiss_1_marr, emiss_1_child, emiss_1_left), 
                          list(emiss_2_marr, emiss_2_child, emiss_2_left),
                          list(emiss_3_marr, emiss_3_child, emiss_3_left)),
    initial_probs = list(initial_probs_1, initial_probs_1, initial_probs_2),
    formula = ~sex * cohort, data = biofam3c$covariates, 
    cluster_names = c("Cluster 1", "Cluster 2", "Cluster 3"),
    channel_names = c("Marriage", "Parenthood", "Left home")
)

mhmm_fit <- fit_model(init_mhmm)

summ_mhmm <- summary(mhmm_fit$model)

names(summ_mhmm)
# [1] "logLik"                          "BIC"                            
# [3] "most_probable_cluster"           "coefficients"                   
# [5] "vcov"                            "prior_cluster_probabilities"    
# [7] "posterior_cluster_probabilities" "classification_table"           
# [9] "model" 

summ_mhmm
# Covariate effects :
# Cluster 1 is the reference.
# 
# Cluster 2 :
#                           Estimate  Std. error
# (Intercept)                 1.1400       0.176
# sexwoman                   -0.2150       0.241
# cohort1936-1945             0.0829       0.239
# cohort1946-1957             0.1420       0.218
# sexwoman:cohort1936-1945    0.2960       0.329
# sexwoman:cohort1946-1957    0.0715       0.295
# 
# Cluster 3 :
#                           Estimate  Std. error
# (Intercept)                  0.430       0.197
# sexwoman                     0.149       0.263
# cohort1936-1945             -0.647       0.290
# cohort1946-1957             -0.899       0.269
# sexwoman:cohort1936-1945     0.212       0.387
# sexwoman:cohort1946-1957    -0.122       0.356
# 
# Log-likelihood: -12712.65   BIC: 26524.88 
# 
# Means of prior cluster probabilities :
# Cluster 1 Cluster 2 Cluster 3 
#     0.191     0.622     0.187 
# 
# Most probable clusters :
#             Cluster 1  Cluster 2  Cluster 3
# count             310       1364        326
# proportion      0.155      0.682      0.163
# 
# Classification table :
# Mean cluster probabilities (in columns) by the most probable cluster (rows)
# 
#           Cluster 1 Cluster 2 Cluster 3
# Cluster 1    0.8391    0.1606  0.000274
# Cluster 2    0.0848    0.8634  0.051819
# Cluster 3    0.0169    0.0499  0.933151

# Plot mixture hidden Markov model
# Interactive plot, one cluster at a time
plot(mhmm_fit$model, interactive = TRUE)

#### HMM for accelerometer ####
### Example 1 (traditional approach)
### Solution of the cut-off point method
### Fictitious activity counts 
x <- c(1,16,19,34,22,6,3,5,6,3,4,1,4,3,5,7,9,8,11,11,
       14,16,13,11,11,10,12,19,23,25,24,23,20,21,22,22,18,7,
       5,3,4,3,2,3,4,5,4,2,1,3,4,5,4,5,3,5,6,4,3,6,4,8,9,12,
       9,14,17,15,25,23,25,35,29,36,34,36,29,41,42,39,40,43,
       37,36,20,20,21,22,23,26,27,28,25,28,24,21,25,21,20,21,
       11,18,19,20,21,13,19,18,20,7,18,8,15,17,16,13,10,4,9,
       7,8,10,9,11,9,11,10,12,12,5,13,4,6,6,13,8,9,10,13,13,
       11,10,5,3,3,4,9,6,8,3,5,3,2,2,1,3,5,11,2,3,5,6,9,8,5,
       2,5,3,4,6,4,8,15,12,16,20,18,23,18,19,24,23,24,21,26,
       36,38,37,39,45,42,41,37,38,38,35,37,35,31,32,30,20,39,
       40,33,32,35,34,36,34,32,33,27,28,25,22,17,18,16,10,9,
       5,12,7,8,8,9,19,21,24,20,23,19,17,18,17,22,11,12,3,9,
       10,4,5,13,3,5,6,3,5,4,2,5,1,2,4,4,3,2,1)
traditional_cut_off_point_method <- cut_off_point_method(x=x,
                                                         cut_points = c(5,15,23),
                                                         names_activity_ranges = c("SED","LIG","MOD","VIG"),
                                                         bout_lengths = c(1,1,2,4,5,10,11,20,21,60,61,260),
                                                         plotting = 1)

### Examples 2,3 and 4 (new approach
### Solution of the HMM based cut-off point method
### Demonstrated both in three steps (Example 2
### and condensed in one function (Examples 3 and 4

### Example 2) Manually in three steps
### Fictitious activity counts

x <- c(1,16,19,34,22,6,3,5,6,3,4,1,4,3,5,7,9,8,11,11,
       14,16,13,11,11,10,12,19,23,25,24,23,20,21,22,22,18,7,
       5,3,4,3,2,3,4,5,4,2,1,3,4,5,4,5,3,5,6,4,3,6,4,8,9,12,
       9,14,17,15,25,23,25,35,29,36,34,36,29,41,42,39,40,43,
       37,36,20,20,21,22,23,26,27,28,25,28,24,21,25,21,20,21,
       11,18,19,20,21,13,19,18,20,7,18,8,15,17,16,13,10,4,9,
       7,8,10,9,11,9,11,10,12,12,5,13,4,6,6,13,8,9,10,13,13,
       11,10,5,3,3,4,9,6,8,3,5,3,2,2,1,3,5,11,2,3,5,6,9,8,5,
       2,5,3,4,6,4,8,15,12,16,20,18,23,18,19,24,23,24,21,26,
       36,38,37,39,45,42,41,37,38,38,35,37,35,31,32,30,20,39,
       40,33,32,35,34,36,34,32,33,27,28,25,22,17,18,16,10,9,
       5,12,7,8,8,9,19,21,24,20,23,19,17,18,17,22,11,12,3,9,
       10,4,5,13,3,5,6,3,5,4,2,5,1,2,4,4,3,2,1)

## Step 1: Training of a HMM
## for the given time-series of counts
## More precisely: training of a poisson
## distribution based hidden Markov model for
## number of states m=2,...,6
## and selection of the model with the most
## plausible m
## TIME CONSUMING
## Not run:
m_trained_HMM <- HMM_training(x = x,
                              min_m = 2,
                              max_m = 6,
                              distribution_class = "pois")$trained_HMM_with_selected_m

## Step 2: Decoding of the trained HMM for the given
## time-series of accelerometer counts to extract
## hidden PA-levels
hidden_PA_levels <- HMM_decoding(x = x,
                                 m = m_trained_HMM$m,
                                 delta = m_trained_HMM$delta,
                                 gamma = m_trained_HMM$gamma,
                                 distribution_class = m_trained_HMM
                                 $distribution_class,
                                 distribution_theta = m_trained_HMM$
                                     distribution_theta)$decoding_distr_means

## Step 3: Assigning of user-sepcified activity ranges
## to the accelerometer counts via the total
## magnitudes of their corresponding
## hidden PA-levels
## In this example four activity ranges
## (named as "sedentary", "light", "moderate
## and "vigorous" physical activity) are
## separated by the three cut-points 5, 15 and 23
HMM_based_cut_off_point_method <- cut_off_point_method(x = x,
                                                       hidden_PA_levels = hidden_PA_levels,
                                                       cut_points = c(5,15,23),
                                                       names_activity_ranges = c("SED","LIG","MOD","VIG"),
                                                       bout_lengths = c(1,1,2,4,5,10,11,20,21,60,61,260),
                                                       plotting=1)

## Example 3) In a single function (Step 1-3 of Example 2
## combined in one function
## Fictitious activity counts

x <- c(1,16,19,34,22,6,3,5,6,3,4,1,4,3,5,7,9,8,11,11,
       14,16,13,11,11,10,12,19,23,25,24,23,20,21,22,22,18,7,
       5,3,4,3,2,3,4,5,4,2,1,3,4,5,4,5,3,5,6,4,3,6,4,8,9,12,
       9,14,17,15,25,23,25,35,29,36,34,36,29,41,42,39,40,43,
       37,36,20,20,21,22,23,26,27,28,25,28,24,21,25,21,20,21,
       11,18,19,20,21,13,19,18,20,7,18,8,15,17,16,13,10,4,9,
       7,8,10,9,11,9,11,10,12,12,5,13,4,6,6,13,8,9,10,13,13,
       11,10,5,3,3,4,9,6,8,3,5,3,2,2,1,3,5,11,2,3,5,6,9,8,5,
       2,5,3,4,6,4,8,15,12,16,20,18,23,18,19,24,23,24,21,26,
       36,38,37,39,45,42,41,37,38,38,35,37,35,31,32,30,20,39,
       40,33,32,35,34,36,34,32,33,27,28,25,22,17,18,16,10,9,
       5,12,7,8,8,9,19,21,24,20,23,19,17,18,17,22,11,12,3,9,
       10,4,5,13,3,5,6,3,5,4,2,5,1,2,4,4,3,2,1)

## Use a (m=4 state) hidden Markov model based on the
## generalized poisson distribution to assign an
## activity range to the counts
## In this example three activity ranges
## (named as "light", "moderate" and "vigorous" physical
## activity) are separated by the two cut-points 15 and 23

## TIME CONSUMING:

HMM_based_cut_off_point_method <- HMM_based_method(x = x,
                                                   cut_points = c(15,23),
                                                   min_m = 4,
                                                   max_m = 4,
                                                   names_activity_ranges = c("LIG","MOD","VIG"),
                                                   distribution_class = "genpois",
                                                   training_method = "numerical",
                                                   DNM_limit_accuracy = 0.05,
                                                   DNM_max_iter = 10,
                                                   bout_lengths = c(1,1,2,4,5,10,11,20,21,60,61,260),
                                                   plotting = 1)


## Example 4) In a single function (Step 1-3 of Example 2
## combined in one function
## (large and highly scatterd time-series
### Generate a large time-series of highly scattered counts
x <- HMM_simulation(
    size = 1500,
    m = 10,
    gamma = 0.93 * diag(10) + rep(0.07 / 10, times = 10),
    distribution_class = "norm",
    distribution_theta = list(mean = c(10, 100, 200, 300, 450,
                                       600, 700, 900, 1100, 1300, 1500),
                              sd=c(rep(100,times=10))),
    obs_round=TRUE,
    obs_non_neg=TRUE,
    plotting=5)$observations

### Compare results of the tradional cut-point method
### and the (6-state-normal-)HMM based method

traditional_cut_off_point_method <- cut_off_point_method(x=x,
                                                         cut_points = c(200,500,1000),
                                                         names_activity_ranges = c("SED","LIG","MOD","VIG"),
                                                         bout_lengths = c(1,1,2,4,5,10,11,20,21,60,61,260),
                                                         plotting = 1)
## TIME CONSUMING:
HMM_based_cut_off_point_method <- HMM_based_method(x=x,
                                                   max_scaled_x = 200,
                                                   cut_points = c(200,500,1000),
                                                   min_m = 6,
                                                   max_m = 6,
                                                   BW_limit_accuracy = 0.5,
                                                   BW_max_iter = 10,
                                                   names_activity_ranges = c("SED","LIG","MOD","VIG"),
                                                   distribution_class = "norm",
                                                   bout_lengths = c(1,1,2,4,5,10,11,20,21,60,61,260),
                                                   plotting = 1)

#### HMM for continious obsrvations ####
# Step-by-step analysis example.
Returns<-logreturns(Prices) # Getting a stationary process
Returns<-Returns*10 # Scaling the values
hmm<-hmmsetcont(Returns) # Creating a HMM object
print(hmm) # Checking the initial parameters
for(i in 1:6){hmm<-baumwelchcont(hmm)} # Baum-Welch is
# executed 6 times and results are accumulated
print(hmm) # Checking the accumulated parameters
summary(hmm) # Getting more detailed information
hmmcomplete<-viterbicont(hmm) # Viterbi execution
statesDistributionsPlot(hmmcomplete, sc=10) # PDFs of
# the whole data set and two states are plotted
par(mfrow=c(2,1))
plot(hmmcomplete, Prices, ylabel="Price")
plot(hmmcomplete, ylabel="Returns") # the reveale

sim<-hmmcontSimul(hmmcomplete, n=100) # simulating the processes
plot(sim$StateProcess1, type="l", ylab="State 1 Process")
plot(sim$StateProcess2, type="l", ylab="State 2 Process")
plot(sim$MarkovChain, type="l", ylab="Markov chain")
plot(sim$SimulatedObservation, type="l", ylab="Full HMM Process")

#### HMM package try ####
a <- unlist (strsplit (gsub ("[^a-z]", "_", tolower (corpus)), ""))
letter.labels=c("_", letters)
prob <- function (x) {x / sum (x)}  # Makes it a probability (it sums to 1)
hmm <- initHMM (c("A", "B"), letter.labels, startProbs=(prob (runif (2))),
                transProbs=apply (matrix (runif (4), 2), 1, prob),
                emissionProbs=apply (matrix (runif (54), 2), 1, prob))

xyplot (a.bw$hmm$emissionProbs[1,] ~ c(1:27), scales=list(x=list(at=1:27, labels=colnames (a.bw$hmm$emissionProbs))), type="h")
xyplot (a.bw$hmm$emissionProbs[2,] ~ c(1:27), scales=list(x=list(at=1:27, labels=colnames (a.bw$hmm$emissionProbs))), type="h")

#### Tutorial in HMM - depmixS4 ####

library(depmixS4)
library(TTR)

## Bull and Bear Markets ##
# Load S&P 500 returns
Sys.setenv(tz = "UTC")
sp500 <- getYahooData("^GSPC", start = 19500101, end = 20120909, freq = "daily")

# Preprocessing
ep <- endpoints(sp500, on = "months", k = 1)
sp500 <- sp500[ep[2:(length(ep)-1)]]
sp500$logret <- log(sp500$Close) - lag(log(sp500$Close))
sp500 <- na.exclude(sp500)

# Plot the S&P 500 returns
plot(sp500$logret, main = "S&P 500 log Returns")

# Regime switching model
mod <- depmix(logret ~ 1, family = gaussian(), nstates = 4, data = sp500)
set.seed(1)
fm2 <- fit(mod, verbose = FALSE)
# Initial probabilities
summary(fm2, which = "prior")
# Transition probabilities
summary(fm2, which = "transition")
# Reponse/emission function
summary(fm2, which = "response")

# Classification (inference task)
tsp500 <- as.ts(sp500)
pbear <- as.ts(posterior(fm2)[,2])
tsp(pbear) <- tsp(tsp500)
plot(cbind(tsp500[, 6], pbear),
     main = "Posterior Probability of State=1 (Volatile, Bear Market)")

map.bear <- as.ts(posterior(fm2)[, 1] == 1)
tsp(map.bear) <- tsp(tsp500)
plot(cbind(tsp500[, 6], map.bear),
     main = "Maximum A Posteriori (MAP) State Sequence")

#### Pakage msm ####
## Simulate data from a Markov model
nsubj <- 30; nobspt <- 5
sim.df <- data.frame(subject = rep(1:nsubj, each=nobspt),
                     time = seq(0, 20, length=nobspt))
set.seed(1)
two.q <- rbind(c(-0.1, 0.1), c(0, 0))
dat <- simmulti.msm(sim.df[,1:2], qmatrix=two.q, drop.absorb=FALSE)

### EXAMPLE 1
## Generate two observations at each time from the same outcome
## distribution:
## Bin(40, 0.1) for state 1, Bin(40, 0.5) for state 2
dat$obs1[dat$state==1] <- rbinom(sum(dat$state==1), 40, 0.1)
dat$obs2[dat$state==1] <- rbinom(sum(dat$state==1), 40, 0.1)
dat$obs1[dat$state==2] <- rbinom(sum(dat$state==2), 40, 0.5)
dat$obs2[dat$state==2] <- rbinom(sum(dat$state==2), 40, 0.5)
dat$obs <- cbind(obs1 = dat$obs1, obs2 = dat$obs2)
## Fitted model should approximately recover true parameters
tmp <- msm(obs ~ time, subject=subject, data=dat, qmatrix=two.q,
           hmodel = list(hmmBinom(size=40, prob=0.2),
                         hmmBinom(size=40, prob=0.2)))

### EXAMPLE 2
## Generate two observations at each time from different
## outcome distributions:
## Bin(40, 0.1) and Bin(40, 0.2) for state 1,
dat$obs1 <- dat$obs2 <- NA
dat$obs1[dat$state==1] <- rbinom(sum(dat$state==1), 40, 0.1)
dat$obs2[dat$state==1] <- rbinom(sum(dat$state==1), 40, 0.2)
## Bin(40, 0.5) and Bin(40, 0.6) for state 2
dat$obs1[dat$state==2] <- rbinom(sum(dat$state==2), 40, 0.6)
dat$obs2[dat$state==2] <- rbinom(sum(dat$state==2), 40, 0.5)
dat$obs <- cbind(obs1 = dat$obs1, obs2 = dat$obs2)
## Fitted model should approximately recover true parameters
msm(obs ~ time, subject=subject, data=dat, qmatrix=two.q,
    hmodel = list(hmmMV(hmmBinom(size=40, prob=0.3),
                        hmmBinom(size=40, prob=0.3)),
                  hmmMV(hmmBinom(size=40, prob=0.3),
                        hmmBinom(size=40, prob=0.3))),
    control=list(maxit=10000))
