# Load necessary libraries
library(ggplot2)
library(ggpubr)
library(tidyr)
library(dplyr)
library(tibble)

filename = 'ElectricityResponse_05-02-2025.csv' 
dir = dirname(rstudioapi::getSourceEditorContext()$path)
folder_figs = file.path(dir, 'figures')
dir.create(folder_figs, showWarnings = FALSE)

##################
### long-term pre-event
##################
# Set default folder
data <- read.csv(file.path(dir, filename))

##tells us the structure, including column names
str(data)

max_value = 14

# subset columns we want
data <- data %>% select(
  "response_id",
  "long.pre.event.section..Modeling.and.simulation.activities.to.understand.the.impacts.of.GICs.on.your.network",
  "long.pre.event.section..Studied.operating.procedures.for.handling.extreme.voltage.fluctuation",
  "long.pre.event.section..Reviewed.manufacturer.simulations.for.transformer.heating.from.GICs",
  "long.pre.event.section..Implemented.extra.cooling.systems.to.prevent.transformer.heating.overheating.due.to.GICs",
  "long.pre.event.section..Re.adjusted.the.negative.sequence.current.protection.due.to.higher.than.normal.harmonics",
  "long.pre.event.section..Ensured.smaller.structures.are.adequately.rated..e.g...capacitors..convertors.etc..",
  "long.pre.event.section..Adjusted.protection.systems..e.g...transformer.relays..to.deal.with.excessive.harmonics",
  "long.pre.event.section..Developed.an.inventory.of.assets.which.could.be.damaged.by.GICs",
  "long.pre.event.section..Purchased.backup.assets.for.those.components.which.could.be.damaged.by.GICs",
  "long.pre.event.section..Prepared.for.how.to.replace.a.transformer.potentially.damaged.by.GICs",
  "long.pre.event.section..Reviewed.the.type..condition.and.locations.of.standby.spare.transformers",
  "long.pre.event.section..Used.past.data.to.understand.reactive.demand.placed.on.transformers.by.GICs",
  "long.pre.event.section..Practiced..blackstart..procedures.in.case.of.an.outage"
)

data_long <- data %>%
  pivot_longer(cols = -c(response_id),
               names_to = "Response_Type",
               values_to = "Response_Value")

folder <- dirname(rstudioapi::getSourceEditorContext()$path)
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

# data$Response_Type[is.na(data$Response_Type)] <- 'None of The Above'

data$Response_Type = factor(data$Response_Type,
                            levels=c(
                              "long.pre.event.section..Modeling.and.simulation.activities.to.understand.the.impacts.of.GICs.on.your.network",
                              "long.pre.event.section..Studied.operating.procedures.for.handling.extreme.voltage.fluctuation",
                              "long.pre.event.section..Reviewed.manufacturer.simulations.for.transformer.heating.from.GICs",
                              "long.pre.event.section..Implemented.extra.cooling.systems.to.prevent.transformer.heating.overheating.due.to.GICs",
                              "long.pre.event.section..Re.adjusted.the.negative.sequence.current.protection.due.to.higher.than.normal.harmonics",
                              "long.pre.event.section..Ensured.smaller.structures.are.adequately.rated..e.g...capacitors..convertors.etc..",
                              "long.pre.event.section..Adjusted.protection.systems..e.g...transformer.relays..to.deal.with.excessive.harmonics",
                              "long.pre.event.section..Developed.an.inventory.of.assets.which.could.be.damaged.by.GICs",
                              "long.pre.event.section..Purchased.backup.assets.for.those.components.which.could.be.damaged.by.GICs",
                              "long.pre.event.section..Prepared.for.how.to.replace.a.transformer.potentially.damaged.by.GICs",
                              "long.pre.event.section..Reviewed.the.type..condition.and.locations.of.standby.spare.transformers",
                              "long.pre.event.section..Used.past.data.to.understand.reactive.demand.placed.on.transformers.by.GICs",
                              "long.pre.event.section..Practiced..blackstart..procedures.in.case.of.an.outage"
                            ),
                            labels=c(
                              "Modeling and simulation activities to understand\nthe impacts of GICs on your network",
                              "Studied operating procedures for\nhandling extreme voltage fluctuation",
                              "Reviewed manufacturer simulations for\ntransformer heating from GICs",
                              "Implemented extra cooling systems to prevent\ntransformer heating/overheating due to GICs",
                              "Re-adjusted the negative-sequence-current\nprotection due to higher-than-normal harmonics",
                              "Ensured smaller structures are adequately\nrated (e.g., capacitors, convertors etc.)",
                              "Adjusted protection systems (e.g., transformer\nrelays) to deal with excessive harmonics",
                              "Developed an inventory of assets which\ncould be damaged by GICs",
                              "Purchased backup assets for those components\nwhich could be damaged by GICs",
                              "Prepared for how to replace a transformers\npotentially damaged by GICs",
                              "Reviewed the type, condition and locations\nof standby spare transformers",
                              "Used past data to understand reactive demand\nplaced on transformers by GICs",
                              "Practiced 'blackstart' procedures in\ncase of an outage"
                            )
)

data$employees = factor(data$employees,
                        levels=c(
                          "lt250",
                          "250-1000",
                          "1000-5000",
                          "5000-10000",
                          "gt10000",
                          ""
                        ),
                        labels=c(
                          "<250",
                          "250-1,000",
                          "1,000-5,000",
                          "5,000-10,000",
                          ">10,000",
                          "Rather Not Say"
                        )
)
data = data[data$employees != "Rather Not Say",]

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

# Summarize the data to get total sum per Response_Type
labels <- summarised_data %>%
  group_by(Response_Type) %>%
  summarise(group_sum = sum(Response_Value, na.rm = TRUE)) %>%
  ungroup()

# Plot data
plot1 = ggplot(summarised_data, aes(x = Response_Type, y = Response_Value)) +
  geom_bar(aes(fill = employees), stat = "identity") + 
  geom_text(data = labels, aes(x = Response_Type, y = group_sum, label = group_sum), 
            vjust = .4, hjust = -1, size = 4)+ 
  coord_flip() +
  theme_minimal() +
  theme(legend.position = 'bottom') +
  labs(colour=NULL,
       title = "Power: Long-Term Pre-Event Decisions Taken",
       subtitle = "Decisions taken before a space weather forecast has been issued.",
       x = NULL, y = "Responses",
       fill="Number of Employees") +
  theme(panel.spacing = unit(0.6, "lines")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)), limits=c(0,max_value)) +
  scale_fill_viridis_d(direction=1)

##################
### short-term pre-event
##################
# Set default folder
folder <- dirname(rstudioapi::getSourceEditorContext()$path)


# load in data
data <- read.csv(file.path(folder, filename))

##inspect data structure, including column names
str(data)

# subset columns we want
data <- data %>% select(
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

data <- read.csv(file.path(folder, filename))

colnames(data)[colnames(data) == "survey.section..How.many.employees.does.your.organization.have."] <- "employees"
colnames(data)[colnames(data) == "survey.section..How.many.customers.does.your.organization.have."] <- "customers"
colnames(data)[colnames(data) == "survey.section..Approximately..how.many.Megawatt.Hours..MWh..does.your.organization.sell.transfer.per.year."] <- "mwh_sold_per_year"
colnames(data)[colnames(data) == "survey.section..Approximately..what.is.your.peak.load.in.Megawatt..MW.."] <- "peak_load_mw"
colnames(data)[colnames(data) == "survey.section..What.is.the.highest.AC.voltage.in.your.system."] <- "highest_ac_voltage"
colnames(data)[colnames(data) == "survey.section..Which.region.do.you.operate.in."] <- "region"

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
                              "Reduced loads on transformers,\nlines and other assets",
                              "Implemented line switching and\nrearrangement",
                              "Activated GIC blocking devices\nfor vulnerable transformers",
                              "Network reconfiguration to remove vulnerable\ntransformers, lines and other assets",
                              "Network reconfiguration to bring more\ntransformers, lines and other assets online",
                              "Increased reactive power reserves",
                              "Brought more generation capacity online",
                              "Ceased power transfers to other networks",
                              "Canceled routine maintenance\nof network to bring more assets online",
                              "Canceled routine maintenance\nof network to free up maintenance teams",
                              "Placed staff teams on alert\nto implement existing preparation plans",
                              "Canceled employee\ntime-off",
                              "Increased staff situational awareness\nof the incoming space weather event"
                            )
)

data$employees = factor(data$employees,
                        levels=c(
                          "lt250",
                          "250-1000",
                          "1000-5000",
                          "5000-10000",
                          "gt10000",
                          ""
                        ),
                        labels=c(
                          "<250",
                          "250-1,000",
                          "1,000-5,000",
                          "5,000-10,000",
                          ">10,000",
                          "Rather Not Say"
                        )
)
data = data[data$employees != "Rather Not Say",]

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

# Summarize the data to get total sum per Response_Type
labels <- summarised_data %>%
  group_by(Response_Type) %>%
  summarise(group_sum = sum(Response_Value, na.rm = TRUE)) %>%
  ungroup()

# max_value =max(summarised_data$Response_Value)

# plot data
plot2 = ggplot(summarised_data, 
               aes(x = Response_Type, y = Response_Value)) +
  geom_bar(aes(fill = employees), stat = "identity") + 
  geom_text(data = labels, aes(x = Response_Type, y = group_sum, label = group_sum), 
          vjust = .4, hjust = -1, size = 4)+ 
  coord_flip() +
  theme_minimal() +
  theme(legend.position = 'bottom') +
  labs(colour=NULL,
       title = "Power: Short-Term Pre-Event Decisions Taken",
       subtitle = "Decisions taken once a space weather forecast has been issued.",
       x = NULL, y = "Responses",
       fill="Number of Employees") +
  theme(panel.spacing = unit(0.6, "lines")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)),
                     limits=c(0,max_value)) +
  scale_fill_viridis_d(direction=1) 

##################
### During the event
##################       
# Set default folder
folder <- dirname(rstudioapi::getSourceEditorContext()$path)

data <- read.csv(file.path(folder, filename))

##tells us the structure, including column names
str(data)

# subset columns we want
data <- data %>% select(
  "response_id",
  "during.event.section..Reduced.loads.on.transformers..lines.and.other.assets..load.reduction.",
  "during.event.section..Implemented.line.switching.rearrangement",
  "during.event.section..Activated.GIC.blocking.devices.for.vulnerable.transformers",
  "during.event.section..Network.reconfiguration.to.take.certain.vulnerable.transformers..lines.and.other.assets.offline",
  "during.event.section..Network.reconfiguration.to.bring.more.transformers..lines.and.other.assets.online",
  "during.event.section..Increased.reactive.power.reserves",
  "during.event.section..Brought.more.generation.capacity.online..if.available.within.your.network.",
  "during.event.section..Ceased.power.transfers.to.other.networks",
  "during.event.section..Canceled.routine.maintenance.of.network.to.bring.more.assets.online",
  "during.event.section..Canceled.routine.maintenance.of.network.to.free.up.maintenance.teams",
  "during.event.section..Placed.staff.teams.on.alert.to.implement.existing.preparation.plans",
  "during.event.section..Canceled.employee.time.off",
  "during.event.section..Increased.staff.situational.awareness.of.the.incoming.space.weather.event..e.g...enhanced.monitoring.of.products.available.from.space.weather.forecasters."
  #"during.event.section..If.you.carried.out.options.not.listed.here..please.report.them.in.the.box.below"#
)

data_long <- data %>%
  pivot_longer(cols = -c(response_id),
               names_to = "Response_Type",
               values_to = "Response_Value")

folder <- dirname(rstudioapi::getSourceEditorContext()$path)

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
                              "during.event.section..Reduced.loads.on.transformers..lines.and.other.assets..load.reduction.",
                              "during.event.section..Implemented.line.switching.rearrangement",
                              "during.event.section..Activated.GIC.blocking.devices.for.vulnerable.transformers",
                              "during.event.section..Network.reconfiguration.to.take.certain.vulnerable.transformers..lines.and.other.assets.offline",
                              "during.event.section..Network.reconfiguration.to.bring.more.transformers..lines.and.other.assets.online",
                              "during.event.section..Increased.reactive.power.reserves",
                              "during.event.section..Brought.more.generation.capacity.online..if.available.within.your.network.",
                              "during.event.section..Ceased.power.transfers.to.other.networks",
                              "during.event.section..Canceled.routine.maintenance.of.network.to.bring.more.assets.online",
                              "during.event.section..Canceled.routine.maintenance.of.network.to.free.up.maintenance.teams",
                              "during.event.section..Placed.staff.teams.on.alert.to.implement.existing.preparation.plans",
                              "during.event.section..Canceled.employee.time.off",
                              "during.event.section..Increased.staff.situational.awareness.of.the.incoming.space.weather.event..e.g...enhanced.monitoring.of.products.available.from.space.weather.forecasters."
                              #"during.event.section..If.you.carried.out.options.not.listed.here..please.report.them.in.the.box.below"#
                            ),
                            labels=c(
                              "Reduced loads on transformers, lines and\nother assets (load reduction)",
                              "Implemented line switching/rearrangement",
                              "Activated GIC blocking devices for vulnerable\ntransformers",
                              "Network reconfiguration to take vulnerable\ntransformers, lines and other assets offline",
                              "Network reconfiguration to bring more\ntransformers, lines and other assets online",
                              "Increased reactive power reserves",
                              "Brought more generation capacity online (if\navailable within your network)",
                              "Ceased power transfers to other networks",
                              "Canceled routine maintenance of network to\nbring more assets online",
                              "Canceled routine maintenance of network to\nfree up maintenance teams",
                              "Placed staff teams on alert to implement existing\npreparation plans",
                              "Canceled employee time off",
                              "Increased staff situational awareness of the\nincoming space weather event"
                              #"If you carried out options not listed here, please report them in the box below"#
                            )
)

data$employees = factor(data$employees,
                        levels=c(
                          "lt250",
                          "250-1000",
                          "1000-5000",
                          "5000-10000",
                          "gt10000",
                          ""
                        ),
                        labels=c(
                          "<250",
                          "250-1,000",
                          "1,000-5,000",
                          "5,000-10,000",
                          ">10,000",
                          "Rather Not Say"
                        )
)
data = data[data$employees != "Rather Not Say",]

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

# Summarize the data to get total sum per Response_Type
labels <- summarised_data %>%
  group_by(Response_Type) %>%
  summarise(group_sum = sum(Response_Value, na.rm = TRUE)) %>%
  ungroup()

# plot data
plot3 = ggplot(summarised_data, 
               aes(x = Response_Type, y = Response_Value)) +
  geom_bar(aes(fill = employees), stat = "identity") + 
  geom_text(data = labels, aes(x = Response_Type, y = group_sum, label = group_sum), 
            vjust = .4, hjust = -1, size = 4)+ 
  coord_flip() +
  theme_minimal() +
  theme(legend.position = 'bottom') +
  labs(colour=NULL,
       title = "Power: During-Event Decisions Taken",
       subtitle = "Decisions taken after storm commencement.",
       x = NULL, y = "Responses",
       fill="Employees") +
  theme(panel.spacing = unit(0.6, "lines")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)),
                     limits=c(0,max_value)) +
  scale_fill_viridis_d(direction=1) 

##################
### After the event
##################
# Set default folder
folder <- dirname(rstudioapi::getSourceEditorContext()$path)

data <- read.csv(file.path(folder, filename))

##tells us the structure, including column names
str(data)

# subset columns we want
data <- data %>% select(
  "response_id",
  "post.event.section..Assessed.dissolved.gas.levels.in.exposed.assets",
  "post.event.section..Investigated.unusual.observations.conditions.of.exposed.assets",
  "post.event.section..Removed.Extra.High.Voltage..EHV..transformers.from.service.for.inspection.of.potential.degradation",
  "post.event.section..Placed.an.order.for.new.apparatus.to.replace.exposed.Extra.High.Voltage..EHV..transformers",
  "post.event.section..Liaised.with.other.operators.to.share.data",
  "post.event.section..Liaised.with.other.operators.to.share.experiences",
  "post.event.section..Revised.preparation.plans.for.future.events",
  "post.event.section..Undertaken.new.modeling.and.simulation.activities",
  #"post.event.section..If.you.carried.out.options.not.listed.here..please.report.them.in.the.box.below",#
  "post.event.section..Physically.replaced.exposed.assets.with.new.components....10k.in.value..",
  "post.event.section..Physically.replaced.exposed.assets.with.new.components...10.100k.in.value..",
  "post.event.section..Physically.replaced.exposed.assets.with.new.components...100k..1m.in.value..",
  "post.event.section..Physically.replaced.exposed.assets.with.new.components....1m.in.value.."
)

data_long <- data %>%
  pivot_longer(cols = -c(response_id),
               names_to = "Response_Type",
               values_to = "Response_Value")

folder <- dirname(rstudioapi::getSourceEditorContext()$path)

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
                              "post.event.section..Assessed.dissolved.gas.levels.in.exposed.assets",
                              "post.event.section..Investigated.unusual.observations.conditions.of.exposed.assets",
                              "post.event.section..Removed.Extra.High.Voltage..EHV..transformers.from.service.for.inspection.of.potential.degradation",
                              "post.event.section..Placed.an.order.for.new.apparatus.to.replace.exposed.Extra.High.Voltage..EHV..transformers",
                              "post.event.section..Liaised.with.other.operators.to.share.data",
                              "post.event.section..Liaised.with.other.operators.to.share.experiences",
                              "post.event.section..Revised.preparation.plans.for.future.events",
                              "post.event.section..Undertaken.new.modeling.and.simulation.activities",
                              #"post.event.section..If.you.carried.out.options.not.listed.here..please.report.them.in.the.box.below",#
                              "post.event.section..Physically.replaced.exposed.assets.with.new.components....10k.in.value..",
                              "post.event.section..Physically.replaced.exposed.assets.with.new.components...10.100k.in.value..",
                              "post.event.section..Physically.replaced.exposed.assets.with.new.components...100k..1m.in.value..",
                              "post.event.section..Physically.replaced.exposed.assets.with.new.components....1m.in.value.."
                            ),
                            labels=c(
                              "Assessed dissolved gas levels in exposed\nassets",
                              "Investigated unusual observations/\nconditions of exposed assets",
                              "Removed EHV transformers from service\nfor inspection of potential degradation",
                              "Placed an order for new apparatus to\nreplace exposed EHV transformers",
                              "Liaised with other operators to share data",
                              "Liaised with other operators to share\nexperiences",
                              "Revised preparation plans for future events",
                              "Undertaken new modeling and simulation\nactivities",
                              #"If you carried out options not listed here, please report them in the box below",#
                              "Physically replaced exposed assets with new\ncomponents (<$10k in value)",
                              "Physically replaced exposed assets with new\ncomponents ($10-100k in value)",
                              "Physically replaced exposed assets with new\ncomponents ($100k-$1m in value)",
                              "Physically replaced exposed assets with new\ncomponents (>$1m in value)"
                            )
)

data$employees = factor(data$employees,
                        levels=c(
                          "lt250",
                          "250-1000",
                          "1000-5000",
                          "5000-10000",
                          "gt10000",
                          ""
                        ),
                        labels=c(
                          "<250",
                          "250-1,000",
                          "1,000-5,000",
                          "5,000-10,000",
                          ">10,000",
                          "Rather Not Say"
                        )
)
data = data[data$employees != "Rather Not Say",]

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

# Summarize the data to get total sum per Response_Type
labels <- summarised_data %>%
  group_by(Response_Type) %>%
  summarise(group_sum = sum(Response_Value, na.rm = TRUE)) %>%
  ungroup()

# plot data
plot4 =
  ggplot(summarised_data, 
         aes(x = Response_Type, y = Response_Value)) +
  geom_bar(aes(fill = employees), stat = "identity") + 
  geom_text(data = labels, aes(x = Response_Type, y = group_sum, label = group_sum), 
            vjust = .4, hjust = -1, size = 4)+ 
  coord_flip() +
  theme_minimal() +
  theme(legend.position = 'bottom') +
  labs(colour=NULL,
       title = "Power: Post-Event Decisions Taken",
       subtitle = "Decisions taken after the event has ceased.",
       x = NULL, y = "Responses",
       fill="Employees") +
  theme(panel.spacing = unit(0.6, "lines")) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)),
                       limits=c(0,max_value)) +
  scale_fill_viridis_d(direction=1) 

# ##################
# ### Incidents
# ##################
# # Set default folder
# folder <- dirname(rstudioapi::getSourceEditorContext()$path)
# 
# data <- read.csv(file.path(folder, filename))
# 
# ##tells us the structure, including column names
# str(data)
# 
# # subset columns we want
# data <- data %>% select(
#   "response_id",
#   "incident.event.section..Interruption.of.power.supply.to.customers",
#   "incident.event.section..Power.apparatus.asset.failure",
#   "incident.event.section..Anomalous.protection.relay.tripping",
#   "incident.event.section..Overloading..or.over.temperature..alarms.triggered",
#   "incident.event.section..Temporary.loss.of.satellite.communication.links"
#   #"incident.event.section..If.any.impacts.are.not.listed.here..please.report.them.in.the.box.below"#
# )
# 
# data_long <- data %>%
#   pivot_longer(cols = -c(response_id),
#                names_to = "Response_Type",
#                values_to = "Response_Value")
# 
# folder <- dirname(rstudioapi::getSourceEditorContext()$path)
# 
# data <- read.csv(file.path(folder, filename))
# 
# colnames(data)[colnames(data) == "survey.section..How.many.employees.does.your.organization.have."] <- "employees"
# colnames(data)[colnames(data) == "survey.section..How.many.customers.does.your.organization.have."] <- "customers"
# colnames(data)[colnames(data) == "survey.section..Approximately..how.many.Megawatt.Hours..MWh..does.your.organization.sell.transfer.per.year."] <- "mwh_sold_per_year"
# colnames(data)[colnames(data) == "survey.section..Approximately..what.is.your.peak.load.in.Megawatt..MW.."] <- "peak_load_mw"
# colnames(data)[colnames(data) == "survey.section..What.is.the.highest.AC.voltage.in.your.system."] <- "highest_ac_voltage"
# colnames(data)[colnames(data) == "survey.section..Which.region.do.you.operate.in."] <- "region"
# 
# 
# str(data)
# 
# # subset columns we want
# data <- data %>% select(
#   "response_id",
#   "employees",
#   "customers",
#   "mwh_sold_per_year",
#   "peak_load_mw",
#   "highest_ac_voltage",
#   "region"
# )
# 
# #merge long dataframe with company characteristics
# data <- merge(data_long, data, by = "response_id")
# 
# #remove old df
# rm(data_long)
# 
# data$Response_Type = factor(data$Response_Type,
#                             levels=c(
#                               "incident.event.section..Interruption.of.power.supply.to.customers",
#                               "incident.event.section..Power.apparatus.asset.failure",
#                               "incident.event.section..Anomalous.protection.relay.tripping",
#                               "incident.event.section..Overloading..or.over.temperature..alarms.triggered",
#                               "incident.event.section..Temporary.loss.of.satellite.communication.links"
#                               #"incident.event.section..If.any.impacts.are.not.listed.here..please.report.them.in.the.box.below"#
#                             ),
#                             labels=c(
#                               "Interruption of power supply to customers",
#                               "Power apparatus/asset failure",
#                               "Anomalous protection relay tripping",
#                               "Overloading, or over temperature, alarms triggered",
#                               "Temporary loss of satellite communication links"
#                               #"If any impacts are not listed here, please report them in the box below"#
#                             )
# )
# 
# data$employees = factor(data$employees,
#                         levels=c(
#                           "lt250",
#                           "250-1000",
#                           "1000-5000",
#                           "5000-10000",
#                           "gt10000",
#                           ""
#                         ),
#                         labels=c(
#                           "<250",
#                           "250-1,000",
#                           "1,000-5,000",
#                           "5,000-10,000",
#                           ">10,000",
#                           "Rather Not Say"
#                         )
# )
# 
# #get structure 
# str(data)
# 
# #get columns
# subset = data %>% select(
#   Response_Type, Response_Value,
#   employees
# )
# 
# # Summarise the data
# summarised_data <- subset %>%
#   group_by(Response_Type, employees) %>%
#   summarise(Response_Value = sum(Response_Value, na.rm = TRUE), .groups = "drop")
# 
# # Calculate cumulative totals for the stacked labels
# summarised_data <- summarised_data %>%
#   group_by(Response_Type) %>%
#   mutate(cumulative_total = cumsum(Response_Value)) %>%
#   ungroup()
# 
# # plot data
# plot5 = ggplot(summarised_data, 
#                aes(x = Response_Type, y = Response_Value, fill = employees)) +
#   geom_bar(stat = "identity") + 
#   coord_flip() +
#   theme_minimal() +
#   theme(legend.position = 'bottom') +
#   labs(colour=NULL,
#        title = "(E) Event Incidents",
#        subtitle = "These are decisions taken after a space weather forecast has been issued.",
#        x = NULL, y = "Responses",
#        fill="Employees") +
#   theme(panel.spacing = unit(0.6, "lines")) +
#   scale_fill_viridis_d(direction=1) 

panel <- ggarrange(plot1, plot2, plot3, plot4, #plot5,# plot6,
                   ncol = 2, nrow = 2, #align = c("hv"),
                   common.legend = TRUE,
                   legend='bottom'#,
                   # heights=c(1,.95)
)

ggsave(
  'power_panel.png',
  plot = last_plot(),
  device = "png",
  path=folder_figs,
  units = c("in"),
  width = 15,
  height = 12,
  bg="white"
)

ggsave(
  'power_1_long_term.png',
  plot = plot1,
  device = "png",
  path=folder,
  units = c("in"),
  width = 8,
  height = 6,
  bg="white"
)

ggsave(
  'power_2_short_term.png',
  plot = plot2,
  device = "png",
  path=folder_figs,
  units = c("in"),
  width = 8,
  height = 6,
  bg="white"
)

ggsave(
  'power_3_during_event.png',
  plot = plot3,
  device = "png",
  path=folder_figs,
  units = c("in"),
  width = 8,
  height = 6,
  bg="white"
)

ggsave(
  'power_4_post_event.png',
  plot = plot4,
  device = "png",
  path=folder_figs,
  units = c("in"),
  width = 8,
  height = 6,
  bg="white"
)

