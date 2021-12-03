source('req.R')

dt <- fread('data.csv')
dt[,day := NULL][,month := NULL][,year := NULL][,geoId := NULL][,countryterritoryCode := NULL]
colnames(dt) <- c('DATE','NEW_CASES','NEW_DEATHS','COUNTRY','POPULATION')
dt[,DATE := dmy(DATE)]
dt[,COUNTRY := toupper(COUNTRY)]
dt[,COUNTRY := gsub('_',' ',COUNTRY)]

### ADD LOCKDOWN DATE
lockdown <- fread('lockdown.csv')
lockdown[,LOCKDOWN_DATE := ymd(LOCKDOWN_DATE)]
dt <- merge(lockdown, dt, by = 'COUNTRY', all.x = T)
rm(lockdown)

dt <- dt[order(COUNTRY,DATE)]

### ADD EXTRA COLUMNS
dt[,IS_LOCKDOWN := DATE >= LOCKDOWN_DATE]
dt[,TOTAL_CASES := cumsum(NEW_CASES), by = COUNTRY]
dt[,TOTAL_DEATHS := cumsum(NEW_DEATHS), by = COUNTRY]

dt[,GROWTH_RATE_TOTAL_CASES := TOTAL_CASES / lag(TOTAL_CASES) - 1]
dt[,GROWTH_RATE_NEW_CASES := NEW_CASES / lag(NEW_CASES) - 1]

dt[,GROWTH_RATE_TOTAL_DEATHS := TOTAL_DEATHS / lag(TOTAL_DEATHS) - 1]
dt[,GROWTH_RATE_NEW_DEATHS := NEW_DEATHS / lag(NEW_DEATHS) - 1]

### ADD 'DAYS SINCE'
dt[TOTAL_CASES >= 1,DAYS_SINCE_1 := as.numeric(DATE - min(DATE)), by = COUNTRY]
dt[TOTAL_CASES >= 10,DAYS_SINCE_10 := as.numeric(DATE - min(DATE)), by = COUNTRY]
dt[TOTAL_CASES >= 100,DAYS_SINCE_100 := as.numeric(DATE - min(DATE)), by = COUNTRY]
dt[TOTAL_CASES >= 1000,DAYS_SINCE_1000 := as.numeric(DATE - min(DATE)), by = COUNTRY]
dt[,DAYS_SINCE_LOCKDOWN := as.numeric(DATE - LOCKDOWN_DATE), by = COUNTRY]

### SELECT COUNTRIES
dt <- dt[COUNTRY %in% c('BELGIUM','ITALY','SPAIN','USA','SWITZERLAND','AUSTRIA','CHINA','UNITED STATES OF AMERICA')]

CairoWin()
ggplot(dt,aes(x = DAYS_SINCE_LOCKDOWN, y = NEW_CASES, col=COUNTRY)) + 
  geom_vline(aes(xintercept = 0), size = 1, linetype = 'solid', alpha = 0.8) +
  geom_vline(aes(xintercept = 7), size = 1, linetype = 'dashed', alpha = 0.8) +
  geom_vline(aes(xintercept = 14), size = 1, linetype = 'dashed', alpha = 0.8) +
  geom_vline(aes(xintercept = 21), size = 1, linetype = 'dashed', alpha = 0.8) +
  geom_line() + 
  geom_point() +
  t +
  coord_cartesian(xlim=c(-14,35), ylim = c(1,100000)) +
  scale_color_manual(values = pal) +
  ylab('DAILY NEW CASES (LOG10 SCALE)') +
  xlab('DAYS SINCE LOCKDOWN') +
  scale_y_continuous(trans = 'log10',
                     breaks = trans_breaks('log10', function(x) 10^x))

CairoWin()
ggplot(dt,aes(x = DAYS_SINCE_LOCKDOWN, y = TOTAL_CASES, col=COUNTRY)) + 
  geom_line() + 
  geom_point() +
  t +
  coord_cartesian(xlim=c(-14,35), ylim = c(1,100000)) +
  scale_color_manual(values = pal) +
  ylab('DAILY NEW CASES (LOG10 SCALE)') +
  xlab('DAYS SINCE LOCKDOWN') +
  scale_y_continuous(trans = 'log10',
                     breaks = trans_breaks('log10', function(x) 10^x))

CairoWin()
ggplot(dt,aes(x = DAYS_SINCE_LOCKDOWN, y = TOTAL_DEATHS, col=COUNTRY)) + 
  geom_line() + 
  geom_point() +
  t +
  coord_cartesian(xlim=c(-14,35), ylim = c(1,100000)) +
  scale_color_manual(values = pal) +
  ylab('DAILY NEW CASES (LOG10 SCALE)') +
  xlab('DAYS SINCE LOCKDOWN') +
  scale_y_continuous(trans = 'log10',
                     breaks = trans_breaks('log10', function(x) 10^x))

CairoWin()
ggplot(dt,aes(x = DAYS_SINCE_LOCKDOWN, y = GROWTH_RATE_CASES_TOTAL, col=COUNTRY)) + 
  geom_line() + 
  geom_point() +
  t +
  coord_cartesian(xlim=c(-14,35), ylim = c(0,1)) +
  scale_color_manual(values = pal) +
  ylab('GROWTH RATE OF TOTAL CASES') +
  xlab('DAYS SINCE LOCKDOWN')
  
CairoWin()
ggplot(dt,aes(x = DAYS_SINCE_LOCKDOWN, y = GROWTH_RATE_NEW_CASES, col=COUNTRY)) + 
  geom_ma(n=7, size=1.2, linetype = 'solid') + 
  geom_point() +
  t +
  coord_cartesian(xlim=c(-14,35), ylim = c(-2,2)) +
  scale_color_manual(values = pal) +
  ylab('GROWTH RATE OF NEW CASES') +
  xlab('DAYS SINCE LOCKDOWN')

CairoWin()
ggplot(dt,aes(x = DAYS_SINCE_LOCKDOWN, y = GROWTH_RATE_TOTAL_CASES, col=COUNTRY)) + 
  geom_ma(n=7, size=1.2, linetype = 'solid') + 
  geom_point() +
  t +
  coord_cartesian(xlim=c(-7,35), ylim = c(0,1.5)) +
  scale_color_manual(values = pal) +
  ylab('GROWTH RATE OF NEW CASES') +
  xlab('DAYS SINCE LOCKDOWN')

