# Load necessary libraries
library(ggplot2)
library(ggpubr)
library(tidyr)
library(dplyr)
library(tibble)

# Set default folder
folder <- dirname(rstudioapi::getSourceEditorContext()$path)
filename = 'satellite_responses.csv'

# load in data
Satellite_data <- read.csv(file.path(folder, filename))

##inspect data structure, including column names
str(Satellite_data)

# subset columns we want
Satellite_data <- Satellite_data %>% select(
  "response_id",
  "short.pre.event.section..Reduced.loads.on.transformers..lines.and.other.assets..load.reduction.",
  "short.pre.event.section..Implemented.line.switching.rearrangement",
  "short.pre.event.section..Activated.GIC.blocking.devices.for.vulnerable.transformers",
  "short.pre.event.section..Network.reconfiguration.to.take.certain.vulnerable.transformers..lines.and.other.assets.offline",
  "short.pre.event.section..Network.reconfiguration.to.bring.more.transformers..lines.and.other.assets.online",
  "short.pre.event.section..Increased.reactive.power.reserves",
  "short.pre.event.section..Brought.more.generation.capacity.online..if.available.within.your.network.",
  "short.pre.event.section..Ceased.power.transfers.to.other.networks",
  "short.pre.event.section..Canceled.routine.maintenance.of.network.to.bring.more.assets.online",
  "short.pre.event.section..Canceled.routine.maintenance.of.network.to.free.up.maintenance.teams",
  "short.pre.event.section..Placed.staff.teams.on.alert.to.implement.existing.preparation.plans",
  "short.pre.event.section..Canceled.employee.time.off",
  "short.pre.event.section..Increased.staff.situational.awareness.of.the.incoming.space.weather.event..e.g...enhanced.monitoring.of.products.available.from.space.weather.forecasters."
)

data_long <- data %>%
  pivot_longer(cols = -c(response_id),
               names_to = "Response_Type",
               values_to = "Response_Value")

folder <- dirname(rstudioapi::getSourceEditorContext()$path)
filename = 'power_responses.csv'
data <- read.csv(file.path(folder, filename))

colnames(data)[colnames(data) == "survey.section..How.many.employees.does.your.organization.have."] <- "employees"
colnames(data)[colnames(data) == "survey.section..How.many.customers.does.your.organization.have."] <- "customers"
colnames(data)[colnames(data) == "survey.section..Approximately..how.many.Megawatt.Hours..MWh..does.your.organization.sell.transfer.per.year."] <- "mwh_sold_per_year"
colnames(data)[colnames(data) == "survey.section..Approximately..what.is.your.peak.load.in.Megawatt..MW.."] <- "peak_load_mw"
colnames(data)[colnames(data) == "survey.section..What.is.the.highest.AC.voltage.in.your.system."] <- "highest_ac_voltage"
colnames(data)[colnames(data) == "survey.section..Which.region.do.you.operate.in."] <- "region"


str(data)

# subset columns we want
data <- data %>% select(
  "response_id",
  "employees",
  "customers",
  "mwh_sold_per_year",
  "peak_load_mw",
  "highest_ac_voltage",
  "region"
)

#merge long dataframe with company characteristics
data <- merge(data_long, data, by = "response_id")

#remove old df
rm(data_long)

data$Response_Type = factor(data$Response_Type,
                            levels=c(
                              "short.pre.event.section..Reduced.loads.on.transformers..lines.and.other.assets..load.reduction.",
                              "short.pre.event.section..Implemented.line.switching.rearrangement",
                              "short.pre.event.section..Activated.GIC.blocking.devices.for.vulnerable.transformers",
                              "short.pre.event.section..Network.reconfiguration.to.take.certain.vulnerable.transformers..lines.and.other.assets.offline",
                              "short.pre.event.section..Network.reconfiguration.to.bring.more.transformers..lines.and.other.assets.online",
                              "short.pre.event.section..Increased.reactive.power.reserves",
                              "short.pre.event.section..Brought.more.generation.capacity.online..if.available.within.your.network.",
                              "short.pre.event.section..Ceased.power.transfers.to.other.networks",
                              "short.pre.event.section..Canceled.routine.maintenance.of.network.to.bring.more.assets.online",
                              "short.pre.event.section..Canceled.routine.maintenance.of.network.to.free.up.maintenance.teams",
                              "short.pre.event.section..Placed.staff.teams.on.alert.to.implement.existing.preparation.plans",
                              "short.pre.event.section..Canceled.employee.time.off",
                              "short.pre.event.section..Increased.staff.situational.awareness.of.the.incoming.space.weather.event..e.g...enhanced.monitoring.of.products.available.from.space.weather.forecasters."
                            ),
                            labels=c(
                              "Reduced loads on transformers, lines and other assets",
                              "Implemented line switching and rearrangement",
                              "Activated GIC blocking devices for vulnerable transformers",
                              "Network reconfiguration to remove vulnerable transformers, lines and other assets",
                              "Network reconfiguration to bring more transformers, lines and other assets online",
                              "Increased reactive power reserves",
                              "Brought more generation capacity online.",
                              "Ceased power transfers to other networks",
                              "Canceled routine maintenance of network to bring more assets online",
                              "Canceled routine.maintenance of network to free up maintenance teams",
                              "Placed staff teams on alert to implement existing preparation plans",
                              "Canceled employee time off",
                              "Increased staff situational awareness of the incoming space weather event"
                            )
)

data$employees = factor(data$employees,
                        levels=c(
                          "lt250",
                          "250-1000",
                          "1000-5000",
                          "5000-10000",
                          "gt10000"
                        ),
                        labels=c(
                          "<250",
                          "250-1,000",
                          "1,000-5,000",
                          "5,000-10,000",
                          ">10,000"
                        )
)

#get structure 
str(data)

#get columns
subset = data %>% select(
  Response_Type, Response_Value,
  employees
)

# Summarise the data
summarised_data <- subset %>%
  group_by(Response_Type, employees) %>%
  summarise(Response_Value = sum(Response_Value, na.rm = TRUE), .groups = "drop")

# Calculate cumulative totals for the stacked labels
summarised_data <- summarised_data %>%
  group_by(Response_Type) %>%
  mutate(cumulative_total = cumsum(Response_Value)) %>%
  ungroup()

# plot data
plot1 = ggplot(summarised_data, 
               aes(x = Response_Type, y = Response_Value, fill = employees)) +
  geom_bar(stat = "identity") + 
  coord_flip() +
  theme_minimal() +
  theme(legend.position = 'bottom') +
  labs(colour=NULL,
       title = "(A) Short-Term Pre-Event Decisions Taken",
       subtitle = "These are decisions taken after a space weather forecast has been issued.",
       x = NULL, y = "Responses",
       fill="Climate Scenario") +
  theme(panel.spacing = unit(0.6, "lines")) +
  scale_fill_viridis_d(direction=1) 


#this is for "short pre-event-section" plot1
#please now do for "long-pre-event-section" plot2
#please now do for "during-event-section" plot3
#please now do for "post-event-section" plot4
#please now do for "incident-event-section" plot5


panel <- ggarrange(plot1, plot1, plot1, plot1, plot1, plot1,
                   ncol = 2, nrow = 3, #align = c("hv"),
                   common.legend = FALSE#,
                   # legend='bottom',
                   # heights=c(1,.95)
)

folder <- dirname(rstudioapi::getSourceEditorContext()$path)
print(folder)
path = file.path(folder, 'panel.png')

ggsave(
  'panel.png',
  plot = last_plot(),
  device = "png",
  path=folder,
  units = c("in"),
  width = 20,
  height = 20,
  bg="white"
)



