load_libs(c("openxlsx","readxl","stringr","dplyr","sqldf","lubridate","ggplot2","ggthemes","ggpubr"))
training_cutoff_yr <- 2010

#FUNCTION TO GET EIA DATA
getEIAdata <- function (filepath, url_prefix, url_suffix, ds) {
  for (i in 1:5){
    filename <- paste0("PADD",toString(i))
    full_url <- paste0(url_prefix, toString(i), url_suffix)
    download.file(url=full_url, destfile=paste0(filepath, filename, ".xls"), mode="wb")
    temp <- read_excel(paste0(filepath, filename, ".xls"), sheet="Data 1", skip=2)
    colnames(temp) <- c("date", ds)
    temp$date <- as.Date(temp$date)
    temp$region <- paste0("PADD", toString(i))
    assign(filename, temp, envir=parent.frame())
  }
}

######################################## ---> GAS PRICES <--- ########################################
#GET GASOLINE PRICE DATA FROM U.S. ENERGY INFORMATION ADMINISTRATION
#https://www.eia.gov/dnav/pet/pet_pri_gnd_dcus_nus_w.htm

getEIAdata("C:/Users/marce/Documents/R/Data/", "https://www.eia.gov/dnav/pet/hist_xls/EMM_EPM0_PTE_R", "0_DPGm.xls", "gas")

#CREATE GAS PRICE DATAFRAME
gas <- rbind(PADD1, PADD2, PADD3, PADD4, PADD5)

##################################### ---> CRUDE OIL PRICES <--- #####################################
#GET CRUDE OIL PRICE DATA FROM U.S. ENERGY INFORMATION ADMINISTRATION
#https://www.eia.gov/dnav/pet/PET_PRI_DFP1_K_M.htm

getEIAdata("C:/Users/marce/Documents/R/Data/", "https://www.eia.gov/dnav/pet/hist_xls/F00", "000__3m.xls", "cop")

#CREATE CRUDE OIL PRICE DATAFRAME
cop <- rbind(PADD1, PADD2, PADD3, PADD4, PADD5)

######################################## ---> OPEC IMPORTS <--- ########################################
#GET OPEC IMPORT DATA FROM U.S. ENERGY INFORMATION ADMINISTRATION
#https://www.eia.gov/dnav/pet/hist/LeafHandler.ashx?n=PET&s=MTTIMXX1&f=M

download.file(url="https://www.eia.gov/dnav/pet/hist_xls/MTTIMXX1m.xls", destfile="C:/Users/marce/Documents/R/Data/opec.xls", mode="wb")

opec <- read_excel("C:/Users/marce/Documents/R/Data/opec.xls", sheet="Data 1", skip=2)
colnames(opec) <- c("date", "opec")
opec$date <- as.Date(opec$date)

####################################### ---> STATE GAS TAX <--- ######################################
#GET STATE GASOLINE TAX DATA
#https://www.fhwa.dot.gov/policyinformation/statistics/2021/mf205.cfm

filename="C:/Users/marce/Documents/R/Data/state.xls"
download.file(url="https://www.fhwa.dot.gov/policyinformation/statistics/2021/xls/mf205.xls", destfile=filename, mode="wb")
state_tax <- read_excel(filename, sheet="GASOLINE", skip=7)
state_tax <- state_tax[1:51, - c(2,3)]
colnames(state_tax) <- c("state", 2000:2021)
colnames(state_tax)[2:ncol(state_tax)] <- paste("Y", colnames(state_tax)[2:ncol(state_tax)], sep="")
state_tax[,2:ncol(state_tax)] <- sapply(state_tax[,2:ncol(state_tax)],as.numeric)
state_tax[,-1] <-round(state_tax[,-1], 1)

#FIX STATE NAMES
state_tax$state[5] <- "California"
state_tax$state[9] <- "District of Columbia"

#ASSIGN REGION
state_tax$region=if_else(state_tax$state %in% 
                           c('Connecticut', 'Maine', 'Massachusetts', 'New Hampshire', 
                             'Rhode Island', 'Vermont','Delaware', 'District of Columbia',
                             'Maryland', 'New Jersey', 'New York', 'Pennsylvania', 'Florida', 
                             'Georgia', 'North Carolina', 'South Carolina', 'Virginia', 'West Virginia'), 'PADD1',
                         if_else(state_tax$state %in%
                                   c('Illinois', 'Indiana', 'Iowa', 'Kansas', 'Kentucky', 'Michigan', 
                                     'Minnesota', 'Missouri', 'Nebraska', 'North Dakota', 'Ohio', 
                                     'Oklahoma', 'South Dakota', 'Tennessee', 'Wisconsin'), 'PADD2', 
                                 if_else(state_tax$state %in%
                                           c('Alabama', 'Arkansas', 'Louisiana', 'Mississippi', 'New Mexico',
                                             'Texas'), 'PADD3', 
                                         if_else(state_tax$state %in%
                                                   c('Colorado', 'Idaho', 'Montana', 'Utah', 'Wyoming'), 'PADD4', 'PADD5'))))

#GET STATE TAX DATA
for (i in 2:(ncol(state_tax)-1)) {
  yr <- 1998 + i
  state_tax_avg <- state_tax %>% group_by(region) %>% summarise_at(i, mean)
  colnames(state_tax_avg)[2] <- "state_tax"
  state_tax_avg$state_tax <- round(state_tax_avg$state_tax, 1)
  state_tax_avg$year <- yr
  assign(paste0("state_avg", i), state_tax_avg)
}

state_taxes <- mget(ls(pattern="^state_avg\\d+")) %>% bind_rows()
state_taxes <- state_taxes[order(state_taxes$year, state_taxes$region), ]

#CREATE DUMMY DATA SET TO HOLD FINAL DATA
monthly_data <- data.frame(date=rep(seq(as.Date("2004-01-15"), as.Date("2023-12-15"), by = "month"), 5), 
                           region=rep(c('PADD1', 'PADD2', 'PADD3', 'PADD4', 'PADD5'), each=240))
monthly_data$year <- year(monthly_data$date)

#ADD STATE TAXES BY YEAR AND REGION
st_tax <- sqldf("SELECT a.*, b.state_tax
                  FROM monthly_data a, state_taxes b
                   WHERE a.year=b.year and a.region=b.region")
st_tax <- st_tax[, c(1,4,2)]

#ADD SUMMER BLEND VARIABLE
gas$summer <- ifelse(month(gas$date) %in% c(5,6,7,8,9), 1, 0)

#CREATE FINAL FILE
gas_model <- sqldf("SELECT a.*, b.cop, c.opec, d.state_tax
                     FROM gas a, cop b, opec c, st_tax d
                      WHERE a.date=b.date and a.region=b.region and
                            a.date=c.date and
                            b.date=d.date and b.region=d.region")

#INFLATION INDEX
download.file(url="https://www2.census.gov/programs-surveys/demo/tables/p60/279/annual-index-value_annual-percent-change.xls", 
              destfile="C:/Users/marce/Documents/R/Data/CPI.xls", mode="wb")
temp <- read_excel("C:/Users/marce/Documents/R/Data/CPI.xls", skip=2)
temp <- temp[1:(nrow(temp)-3), -3]
colnames(temp) <- c("year","CPI")
temp <- temp[which(temp$year>=2004),]

gas_model$yr <- year(gas_model$date)

gas_model$gas_inflation <- NA

#CONVERT PRICE OVER TIME TO REAL DOLLARS BASED ON REFRENCE YEAR
ref_yr=2004
for (i in 1:nrow(gas_model)) {
  gas_model$gas_inflation[i] <- round(gas_model$gas[i]*(temp[which(temp$year==gas_model$yr[i]),]$CPI / temp[which(temp$year==ref_yr),]$CPI), 3)
}

#REMOVE TEMPORARY FILES
dfs <- Filter(function(x) is.data.frame(get(x)), ls())
dfs <- lapply(dfs, grep, pattern = "state_avg", value=TRUE)
remove(list=unlist(dfs))

#TRAINING DATA
train <- subset(gas_model, gas_model$yr<=training_cutoff_yr)

################################### -> CORRELATION TEST <- #########################################
show_scatter <- function(loc, var1, var2) {
  regional <- subset(train, train$region==loc)
  
  x=eval(parse(text=paste0("regional$", var1)))
  y=eval(parse(text=paste0("regional$", var2)))
  print(cor.test(x, y, method="pearson"))
  
  ggscatter(regional, x=var2, y=var1, 
            add = "reg.line", conf.int = TRUE, 
            cor.coef = TRUE, cor.method = "pearson",
            xlab = "", ylab = "", title=paste0(loc, " - Comparing ", var1, " and ", var2), size=1)
}

show_scatter('PADD1', 'gas_inflation', 'cop')
show_scatter('PADD2', 'gas_inflation', 'cop')
show_scatter('PADD3', 'gas_inflation', 'cop')
show_scatter('PADD4', 'gas_inflation', 'cop')
show_scatter('PADD5', 'gas_inflation', 'cop')

show_scatter('PADD1', 'gas_inflation', 'opec')
show_scatter('PADD2', 'gas_inflation', 'opec')
show_scatter('PADD3', 'gas_inflation', 'opec')
show_scatter('PADD4', 'gas_inflation', 'opec')
show_scatter('PADD5', 'gas_inflation', 'opec')

show_scatter('PADD1', 'gas_inflation', 'state_tax')
show_scatter('PADD2', 'gas_inflation', 'state_tax')
show_scatter('PADD3', 'gas_inflation', 'state_tax')
show_scatter('PADD4', 'gas_inflation', 'state_tax')
show_scatter('PADD5', 'gas_inflation', 'state_tax')

show_scatter('PADD1', 'gas_inflation', 'summer')
show_scatter('PADD2', 'gas_inflation', 'summer')
show_scatter('PADD3', 'gas_inflation', 'summer')
show_scatter('PADD4', 'gas_inflation', 'summer')
show_scatter('PADD5', 'gas_inflation', 'summer')

show_scatter('PADD1', 'gas_inflation', 'yr')
show_scatter('PADD2', 'gas_inflation', 'yr')
show_scatter('PADD3', 'gas_inflation', 'yr')
show_scatter('PADD4', 'gas_inflation', 'yr')
show_scatter('PADD5', 'gas_inflation', 'yr')

show_scatter('PADD1', 'cop', 'yr')
show_scatter('PADD2', 'cop', 'yr')
show_scatter('PADD3', 'cop', 'yr')
show_scatter('PADD4', 'cop', 'yr')
show_scatter('PADD5', 'cop', 'yr')

#################################### -> MODEL BUILDING <- ##########################################
model_cutoff_yr <- training_cutoff_yr + 1
model <- subset(gas_model, gas_model$yr>=model_cutoff_yr)

model_by_PADD <- function(loc) {
  getPADD <- subset(model, model$region==loc)
  gasprice <- lm(gas_inflation ~ cop + summer + yr, data=getPADD)
  print(paste0("Region: ", loc))
  print(summary(gasprice)) #MODEL STATISTICS
  print(deviance(gasprice)) #RESIDUAL SUM OF SQUARES FOR MODEL COMPARISON, IF NECESSARY
  print(sum(gasprice$residuals)) #SUM OF THE RESIDUALS
  af <- anova(gasprice)
  afss <- af$"Sum Sq"
  x <- cbind(af,PctExp=afss/sum(afss)*100)
  print(x[,c(2,6)])
  
  estimates <- as.data.frame(predict(gasprice, getPADD))
  colnames(estimates) <- "est"
  estimates$est <- round(estimates$est, 3)
  getPADD <- cbind(getPADD, estimates)
  rownames(getPADD) <- seq(1:nrow(getPADD))
  assign(loc, getPADD, envir=parent.frame())
}

model_by_PADD('PADD1')
model_by_PADD('PADD2')
model_by_PADD('PADD3')
model_by_PADD('PADD4')
model_by_PADD('PADD5')

all <- rbind(PADD1, PADD2, PADD3, PADD4, PADD5)

#CREATE DATAFRAME FOR LOCATION OF GEOM_TEXT()
endpoint1 <- PADD1[1,]
endpoint2 <- PADD2[1,]
endpoint3 <- PADD3[1,]
endpoint4 <- PADD4[1,]
endpoint5 <- PADD5[1,]

endpoint <- rbind(endpoint1, endpoint2, endpoint3, endpoint4, endpoint5)
endpoint$gas_inflationA <- endpoint$gas_inflation + 1.50
endpoint$gas_inflationE <- endpoint$gas_inflation - 0.25

regions = as_labeller(c(
  'PADD1'='PADD1: Northeast',
  'PADD2'='PADD2: Midwest',
  'PADD3'='PADD3: Gulf Coast',
  'PADD4'='PADD4: Rocky Mountain',
  'PADD5'='PADD5: West Coast, AK, HI'
)
)

#CREATE GRAPH IMAGE
png(filename="Gas Prices.png", width=2400, height=6000, pointsize=4, res=300)

ggplot(all, aes(x=date)) +
  geom_line(aes(y=gas_inflation), colour="red", linewidth=0.5) + 
  geom_line(aes(y=est), colour="blue", linewidth=0.5)+ 
  scale_y_continuous(limits=c(0,6), labels=scales::dollar_format()) +
  labs(x="", y="",
       title=paste("\nGas Prices Over Time in Real", ref_yr, "Dollars", sep=" "),
       subtitle=paste(months(model$date[1]), year(model$date[1]), "-", 
                      months(model$date[nrow(model)]), year(model$date[nrow(model)]), "\n", sep=" ")) +
  facet_wrap(~ region, labeller=regions, ncol=1, scales="free", strip.position = "top") + 
  geom_text(data=endpoint, aes(x=date, y=gas_inflationA, label="Actual"), color="red", size=3, hjust=0) +
  geom_text(data=endpoint, aes(x=date, y=gas_inflationE, label="Predicted"), color="blue", size=3, hjust=0) +
  theme(
    panel.border = element_blank(),
    panel.grid.major.y = element_line(colour = "gray80", linewidth=0.25, linetype="dashed"),
    panel.grid.minor = element_line(colour = "white", linewidth = 0.25),
    panel.background = element_rect(fill = "white", colour = NA),
    axis.ticks.length=unit(0, "pt"),
    panel.spacing=unit(10,"lines"),
    strip.text.x = element_text(size=12, face="bold", hjust=0,
                                margin=margin(t=0.1, r=10, b=0.1, l=0.1, "cm")),
    strip.background=element_rect(colour="lemonchiffon", fill="lemonchiffon")
  ) 

dev.off()

