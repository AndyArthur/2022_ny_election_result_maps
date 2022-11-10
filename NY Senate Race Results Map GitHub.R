library(RSelenium)
library(tidyverse)
library(netstat)
library(rvest)
library(ggtext)
# also loaded below, ggredist and arcpullr

rm(list=ls())

system('killall java')

rs <- rsDriver(
  remoteServerAddr='localhost',
  port = free_port(random=T),
  browser = 'firefox',
  extraCapabilities = list("moz:firefoxOptions" = list(args = list('--headless'))),
  verbose = F
)

rsc <- rs$client
rsc$navigate("https://nyenr.elections.ny.gov/")

Sys.sleep(0.5)

# contest
rsc$findElement(using='xpath', str_c('/html/body/div[4]/div/div/form/table[2]/tbody/tr[1]/td[1]/select/option[7]'))$clickElement()

# districts 
rsc$findElement(using='xpath', '/html/body/div[4]/div/div/form/table[2]/tbody/tr[1]/td[2]/select/option[1]')$clickElement()

# county path
rsc$findElement(using='xpath', '/html/body/div[4]/div/div/form/table[2]/tbody/tr[1]/td[3]/select/option[1]')$clickElement()

# click search
rsc$findElement(using='xpath','//*[@id="ctl00_HomePage_btnGo"]')$clickElement()

html <- rsc$getPageSource() %>% unlist() %>% read_html()
tables <- html_table(html)

lastUpdate <- tables[[4]][1,1] %>% 
  as.character() %>% 
  as_datetime(format='%m/%d/%Y  %H:%M:%S %p', tz='EST')

asmResult <- NA
for (table in tables) {
  if (!grepl('\\>',table[[1,1]]) | !grepl('Reporting',table[[1,1]])) next
  
  ad <- str_match(table[[1,1]],'(.*?) >') %>% .[,2]
  
  rpt <- str_match(table[[1,1]],'Reporting: (.*?) of') %>% .[,2] %>% parse_number()
  te <- str_match(table[[1,1]],' of (.*?)$') %>% .[,2] %>% parse_number()
  
  table %>% 
    janitor::row_to_names(3) %>% 
    janitor::clean_names() %>%
    .[-nrow(.),c(-3,-6,-9, -10)] %>%
    mutate(
      across(3:6, ~parse_number(.)),
    ) -> table
  
  
  table <- cbind(district = parse_number(ad), 
                 table
                 )
  
  asmResult <- rbind(asmResult, table)  
  
}

asmResult <- asmResult[-1,]

nyco <- counties('ny', cb=T) %>% st_transform(3857)
nysm <- states(cb=T) %>% filter(STUSPS=='NY') %>% st_transform(3857)
asm <- arcpullr::get_spatial_layer('https://gisservices.its.ny.gov/arcgis/rest/services/NY_2022_Political_District_Boundaries_Based_on_2020_Census/MapServer/2') %>%
  st_transform(3857) %>% rmapshaper::ms_simplify() %>% st_intersection(nysm) 

asmResult %>% 
  filter(!is.na(percent_by_candidate), party %in% c('DEM','REP')) %>%
  pivot_wider(id_cols = district, names_from=party, values_from = percent_by_candidate) %>%
  mutate(DEM=ifelse(is.na(DEM), REP-100, DEM)) %>%
  full_join(asm, ., by=c('DISTRICT'='district')) %>%
  ggplot() + 
  geom_sf(aes(fill=DEM/100), size=0.25) +
  ggredist::scale_fill_party_c() +
  geom_sf(data=nyco, fill=NA, linetype='dashed', alpha=0.3, size=0.1) +
  geom_sf(data=nysm, fill=NA) +
  theme_void() -> main

# NYC Subset
bbox <- nyco %>% filter(NAME %in% c('Richmond','Bronx','Queens')) %>% st_bbox()
nyc <- main + coord_sf(xlim=c(bbox[1], bbox[3]), ylim=c(bbox[2], bbox[4]), expand=T, crs=3857) +
  theme(panel.border = element_rect(fill=NA, size=0.5), legend.position = 'none',
        plot.title=element_text(family='Lato',size=12, margin=margin(b=3))) +
  labs(title='New York City')  

nys <- main + 
  coord_sf(expand=F, crs=3857) +
  labs(title = str_c('<span style="font-size: 60pt">2022 State Senate Races'),
       tag=paste('Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y"),'\nSource: NYS Board of Elections as of ',format(lastUpdate, format="%a %-b %-d %-I:%M %p.")),
       fill = "")  +
  theme(
    legend.key.height = unit(3.5,'cm'),
    legend.key.width = unit(0.5,'cm'),
    legend.position = c(0.9,0.6),
    legend.direction = 'vertical',
    legend.justification = 'center',
    legend.spacing.y = unit(0.5, 'cm'),
    text= element_text(family='Lato',size=14),
    plot.title=element_textbox(halign = 0.5, hjust=0, face='bold',size=20, margin=unit(c(25,0,5,0),'pt'), maxheight=0, width=0.4),
    plot.background = element_rect(fill = "white", color="white"),
    plot.subtitle=element_textbox(hjust=0.5, halign=0.5, margin=unit(c(5,0,5,0),'pt')),
    plot.tag=element_text(size=10,hjust=0, color='#555555'),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0.0,0.01),
  ) 


ggdraw() +
  draw_plot(nys) +
  draw_plot(nyc, x = 0.3, y = 0.04, width = 0.35, height = 0.26)

fn <- str_c('ny-state-senate')
ggsave(paste('~/Desktop/',fn,'.jpg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::jpeg, bg='white')
ggsave(paste('~/Desktop/',fn,'.svg',sep=''), width=1920, height=1200, units='px', dpi=120, device = grDevices::svg, bg='white')
system(paste('scour ~/Desktop/',fn,'.svg ~/Desktop/',fn,'.svgz',sep=''))
unlink(str_c('~/Desktop/',fn,'.svg'))



