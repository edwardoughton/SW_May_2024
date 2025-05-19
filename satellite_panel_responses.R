# Load necessary libraries
library(ggplot2)
library(ggpubr)
library(tidyr)
library(dplyr)
library(tibble)

filename = 'SatelliteResponse_05-02-2025.csv'
dir = dirname(rstudioapi::getSourceEditorContext()$path)
folder_figs = file.path(dir, 'figures')
dir.create(folder_figs, showWarnings = FALSE)

##################
### long-term pre-event
##################
# Set default folder
folder <- dirname(rstudioapi::getSourceEditorContext()$path)
data <- read.csv(file.path(folder, 'data', 'survey_responses', filename))

##tells us the structure, including column names
str(data)

max_value = 15

# subset columns we want
data <- data %>% select(
  "response_id",
  "long.pre.event.section..Modeling.and.simulation.of.satellite.system.design.to.quantify.expected.radiation.exposure.",
  "long.pre.event.section..Modeling.and.simulation.of.orbital.trajectories.to.quantify.expected.radiation.exposure.",
  "long.pre.event.section..Evaluation.of.adequate.satellite.shielding.thickness.and.ongoing.quality.levels.",
  "long.pre.event.section..Developed.design.redundancy.for.onboard.systems.to.protect.against.radiation.exposure.",
  "long.pre.event.section..Monitored.satellite.asset.orbital.trajectory.to.anticipate.which.assets.are.likely.to.receive.the.largest.cumulative.radiation.exposure.",
  "long.pre.event.section..Implemented.power.supply.redundancy.for.ground.based.systems..e.g...Earth.stations..",
  "long.pre.event.section..Implemented.optical.fiber.link.redundancy.for.ground.based.systems..e.g...Earth.stations.."
  #"long.pre.event.section..Please.add.any.other.considerations.we.did.not.list."#
)
data_long <- data %>%
  pivot_longer(cols = -c(response_id),
               names_to = "Response_Type",
               values_to = "Response_Value")

folder <- dirname(rstudioapi::getSourceEditorContext()$path)
data <- read.csv(file.path(folder, 'data', 'survey_responses', filename))

colnames(data)[colnames(data) == "survey.section..How.many.employees.does.your.company.have."] <- "employees"
colnames(data)[colnames(data) == "survey.section..How.many.customers.does.your.organization.have."] <- "customers"
colnames(data)[colnames(data) == "survey.section..How.many.primary.satellites.do.you.operate."] <- "primary_satellites"
colnames(data)[colnames(data) == "survey.section..What.is.the.primary.service.you.offer.support."] <- "primary_service"
colnames(data)[colnames(data) == "survey.section..Which.region.s..do.you.operate.in."] <- "region"
colnames(data)[colnames(data) == "survey.section..Which.country.region.is.your.HQ.located."] <- "country"

str(data)

# subset columns we want
data <- data %>% select(
  "response_id",
  "employees",
  "customers",
  "primary_satellites",
  "primary_service",
  "region",
  "country"
)

#merge long dataframe with company characteristics
data <- merge(data_long, data, by = "response_id")

#remove old df
rm(data_long)

data$Response_Type = factor(data$Response_Type,
                            levels=c(
                              "long.pre.event.section..Modeling.and.simulation.of.satellite.system.design.to.quantify.expected.radiation.exposure.",
                              "long.pre.event.section..Modeling.and.simulation.of.orbital.trajectories.to.quantify.expected.radiation.exposure.",
                              "long.pre.event.section..Evaluation.of.adequate.satellite.shielding.thickness.and.ongoing.quality.levels.",
                              "long.pre.event.section..Developed.design.redundancy.for.onboard.systems.to.protect.against.radiation.exposure.",
                              "long.pre.event.section..Monitored.satellite.asset.orbital.trajectory.to.anticipate.which.assets.are.likely.to.receive.the.largest.cumulative.radiation.exposure.",
                              "long.pre.event.section..Implemented.power.supply.redundancy.for.ground.based.systems..e.g...Earth.stations..",
                              "long.pre.event.section..Implemented.optical.fiber.link.redundancy.for.ground.based.systems..e.g...Earth.stations.."
                              #"long.pre.event.section..Please.add.any.other.considerations.we.did.not.list."#
                            ),
                            labels=c(
                              "Modeling and simulation of satellite system\ndesign to quantify expected radiation\nexposure",
                              "Modeling and simulation of orbital\ntrajectories to quantify expected radiation\nexposure",
                              "Evaluation of adequate satellite shielding\nthickness and ongoing quality levels",
                              "Developed design redundancy for onboard\nsystems to protect against radiation\nexposure",
                              "Monitored satellite asset orbital trajectory to\nanticipate which assets are likely to receive\nthe largest cumulative radiation exposure",
                              "Implemented power supply redundancy for\nground-based systems (e.g., Earth stations)",
                              "Implemented optical fiber link redundancy for\nground-based systems (e.g., Earth stations)"
                              #"Please add any other considerations we did not list:"#
                            )
)

data$employees = factor(data$employees,
                        levels=c(
                          "1-24",
                          "25-99",
                          "100-199",
                          "200-499",
                          "500+",
                           ""
                        ),
                        labels=c(
                          "1-24",
                          "25-99",
                          "100-199",
                          "200-499",
                          ">500",
                          "Not Disclosed"
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

# Summarize the data to get total sum per Response_Type
labels <- summarised_data %>%
  group_by(Response_Type) %>%
  summarise(group_sum = sum(Response_Value, na.rm = TRUE)) %>%
  ungroup()

# Ensure all combinations exist
summarised_data <- summarised_data %>%
  complete(Response_Type, employees, fill = list(Response_Value = 0))

original_levels <- c("1-24", "25-99", "100-199", "200-499", ">500")#, "Not Disclosed")
summarised_data$employees <- factor(summarised_data$employees, levels = rev(original_levels))
summarised_data = summarised_data[complete.cases(summarised_data),]

# plot data
plot1 = ggplot(summarised_data, 
               aes(x = factor(Response_Type, levels = rev(unique(Response_Type))), 
                   y = Response_Value)) +
  geom_bar(aes(fill = employees), stat = "identity") + 
  geom_text(data = labels, aes(x = Response_Type, y = group_sum, label = group_sum), 
            vjust = .4, hjust = -1, size = 4) + 
  coord_flip() +
  guides(fill = guide_legend(nrow = 1, ncol = 10, reverse = TRUE)) +  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    legend.box.just = "center"
  ) +
  labs(
    title = "Satellite: Long-Term Pre-Event Decisions Taken",
    subtitle = "Decisions taken before a space weather forecast has been issued.",
    x = NULL, y = "Responses",
    fill = "Employees"
  ) +
  theme(panel.spacing = unit(0.6, "lines")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)), limits = c(0, max_value)) +
  scale_fill_viridis_d(direction = 1)

##################
### short-term pre-event
##################
# Set default folder
folder <- dirname(rstudioapi::getSourceEditorContext()$path)

# load in data
data <- read.csv(file.path(folder,'data', 'survey_responses', filename))

##inspect data structure, including column names
str(data)

# subset columns we want
data <- data %>% select(
  "response_id",
  "short.pre.event.section..Moved.at.risk.satellites.out.of.expected.orbit.",
  "short.pre.event.section..Placed.satellites.into.safe.mode.",
  "short.pre.event.section..Notified.customers.stakeholders.of.coming.event.and.potential.service.disruption.",
  "short.pre.event.section..Postponed.launch.activities.for.new.satellites.",
  "short.pre.event.section..Canceled.routine.maintenance.of.network..e.g...of.terrestrial.assets..",
  "short.pre.event.section..Canceled.routine.maintenance.of.network.to.free.up.maintenance.teams.",
  "short.pre.event.section..Placed.staff.teams.on.alert.to.implement.existing.preparation.plans.",
  "short.pre.event.section..Canceled.employee.time.off.",
  "short.pre.event.section..Increased.staff.situational.awareness.of.the.incoming.space.weather.event."
  #"short.pre.event.section..Please.add.any.other.considerations.we.did.not.list."#
)

data_long <- data %>%
  pivot_longer(cols = -c(response_id),
               names_to = "Response_Type",
               values_to = "Response_Value")

folder <- dirname(rstudioapi::getSourceEditorContext()$path)
data <- read.csv(file.path(folder, 'data', 'survey_responses', filename))

colnames(data)[colnames(data) == "survey.section..How.many.employees.does.your.company.have."] <- "employees"
colnames(data)[colnames(data) == "survey.section..How.many.customers.does.your.organization.have."] <- "customers"
colnames(data)[colnames(data) == "survey.section..How.many.primary.satellites.do.you.operate."] <- "primary_satellites"
colnames(data)[colnames(data) == "survey.section..What.is.the.primary.service.you.offer.support."] <- "primary_service"
colnames(data)[colnames(data) == "survey.section..Which.region.s..do.you.operate.in."] <- "region"
colnames(data)[colnames(data) == "survey.section..Which.country.region.is.your.HQ.located."] <- "country"

str(data)

# subset columns we want
data <- data %>% select(
  "response_id",
  "employees",
  "customers",
  "primary_satellites",
  "primary_service",
  "region",
  "country"
)

#merge long dataframe with company characteristics
data <- merge(data_long, data, by = "response_id")

#remove old df
rm(data_long)

data$Response_Type = factor(data$Response_Type,
                            levels=c(
                              "short.pre.event.section..Moved.at.risk.satellites.out.of.expected.orbit.",
                              "short.pre.event.section..Placed.satellites.into.safe.mode.",
                              "short.pre.event.section..Notified.customers.stakeholders.of.coming.event.and.potential.service.disruption.",
                              "short.pre.event.section..Postponed.launch.activities.for.new.satellites.",
                              "short.pre.event.section..Canceled.routine.maintenance.of.network..e.g...of.terrestrial.assets..",
                              "short.pre.event.section..Canceled.routine.maintenance.of.network.to.free.up.maintenance.teams.",
                              "short.pre.event.section..Placed.staff.teams.on.alert.to.implement.existing.preparation.plans.",
                              "short.pre.event.section..Canceled.employee.time.off.",
                              "short.pre.event.section..Increased.staff.situational.awareness.of.the.incoming.space.weather.event."
                              #"short.pre.event.section..Please.add.any.other.considerations.we.did.not.list."#
                            ),
                            labels=c(
                              "Moved at-risk satellites out\nof expected orbit",
                              "Placed satellites\ninto safe mode",
                              "Notified customers/stakeholders of coming\nevent and potential service disruption",
                              "Postponed launch activities\nfor new satellites",
                              "Canceled routine maintenance of\nnetwork (e.g., of terrestrial assets)",
                              "Canceled routine maintenance of\nnetwork to free up maintenance teams",
                              "Placed staff teams on alert to\nimplement existing preparation plans",
                              "Canceled employee time off",
                              "Increased staff situational awareness\nof the incoming space weather event."
                              #"Please add any other considerations we did not list:"#
                            )
)

data$employees = factor(data$employees,
                        levels=c(
                          "1-24",
                          "25-99",
                          "100-199",
                          "200-499",
                          "500+",
                          ""
                        ),
                        labels=c(
                          "1-24",
                          "25-99",
                          "100-199",
                          "200-499",
                          ">500",
                          "Not Disclosed"
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

# Summarize the data to get total sum per Response_Type
labels <- summarised_data %>%
  group_by(Response_Type) %>%
  summarise(group_sum = sum(Response_Value, na.rm = TRUE)) %>%
  ungroup()

summarised_data <- summarised_data %>%
  complete(Response_Type, employees, fill = list(Response_Value = 0))

original_levels <- c("1-24", "25-99", "100-199", "200-499", ">500")#, "Not Disclosed")
summarised_data$employees <- factor(summarised_data$employees, levels = rev(original_levels))
summarised_data = summarised_data[complete.cases(summarised_data),]

plot2 = ggplot(summarised_data, 
               aes(x = factor(Response_Type, levels = rev(unique(Response_Type))), 
                   y = Response_Value)) +
  geom_bar(aes(fill = employees), stat = "identity") + 
  geom_text(data = labels, aes(x = Response_Type, y = group_sum, label = group_sum), 
            vjust = .4, hjust = -1, size = 4) + 
  coord_flip() +
  guides(fill = guide_legend(nrow = 1, ncol = 10, reverse = TRUE)) +  # ✅ restore legend order
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    legend.box.just = "center"
  ) +
  labs(
    colour = NULL,
    title = "Satellite: Short-Term Pre-Event Decisions Taken",
    subtitle = "Decisions taken after a space weather forecast has been issued.",
    x = NULL, y = "Responses",
    fill = "Employees") +
  theme(panel.spacing = unit(0.6, "lines")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)),
                     limits = c(0, max_value)) +
  scale_fill_viridis_d(direction = 1)

##################
### During the event
##################       
# Set default folder
folder <- dirname(rstudioapi::getSourceEditorContext()$path)
data <- read.csv(file.path(folder, 'data', 'survey_responses',filename))

##tells us the structure, including column names
str(data)

# subset columns we want
data <- data %>% select(
  "response_id",
  "during.event.section..Moved.at.risk.satellites.out.of.expected.orbit.",
  "during.event.section..Placed.satellites.into.safe.mode.",
  "during.event.section..Notified.customers.stakeholders.of.coming.event.and.potential.service.disruption.",
  "during.event.section..Postponed.launch.activities.for.new.satellites.",
  "during.event.section..Canceled.routine.maintenance.of.network..e.g...of.terrestrial.assets..",
  "during.event.section..Canceled.routine.maintenance.of.network.to.free.up.maintenance.teams.",
  "during.event.section..Placed.staff.teams.on.alert.to.implement.existing.preparation.plans.",
  "during.event.section..Canceled.employee.time.off.",
  "during.event.section..Increased.staff.situational.awareness.of.the.incoming.space.weather.event."
  #"during.event.section..Please.add.any.other.considerations.we.did.not.list."#
)

data_long <- data %>%
  pivot_longer(cols = -c(response_id),
               names_to = "Response_Type",
               values_to = "Response_Value")

folder <- dirname(rstudioapi::getSourceEditorContext()$path)
data <- read.csv(file.path(folder, 'data', 'survey_responses', filename))

colnames(data)[colnames(data) == "survey.section..How.many.employees.does.your.company.have."] <- "employees"
colnames(data)[colnames(data) == "survey.section..How.many.customers.does.your.organization.have."] <- "customers"
colnames(data)[colnames(data) == "survey.section..How.many.primary.satellites.do.you.operate."] <- "primary_satellites"
colnames(data)[colnames(data) == "survey.section..What.is.the.primary.service.you.offer.support."] <- "primary_service"
colnames(data)[colnames(data) == "survey.section..Which.region.s..do.you.operate.in."] <- "region"
colnames(data)[colnames(data) == "survey.section..Which.country.region.is.your.HQ.located."] <- "country"


str(data)

# subset columns we want
data <- data %>% select(
  "response_id",
  "employees",
  "customers",
  "primary_satellites",
  "primary_service",
  "region",
  "country"
)

#merge long dataframe with company characteristics
data <- merge(data_long, data, by = "response_id")

#remove old df
rm(data_long)

data$Response_Type = factor(data$Response_Type,
                            levels=c(
                              "during.event.section..Moved.at.risk.satellites.out.of.expected.orbit.",
                              "during.event.section..Placed.satellites.into.safe.mode.",
                              "during.event.section..Notified.customers.stakeholders.of.coming.event.and.potential.service.disruption.",
                              "during.event.section..Postponed.launch.activities.for.new.satellites.",
                              "during.event.section..Canceled.routine.maintenance.of.network..e.g...of.terrestrial.assets..",
                              "during.event.section..Canceled.routine.maintenance.of.network.to.free.up.maintenance.teams.",
                              "during.event.section..Placed.staff.teams.on.alert.to.implement.existing.preparation.plans.",
                              "during.event.section..Canceled.employee.time.off.",
                              "during.event.section..Increased.staff.situational.awareness.of.the.incoming.space.weather.event."
                              #"during.event.section..Please.add.any.other.considerations.we.did.not.list."#
                            ),
                            labels=c(
                              "Moved at-risk satellites out of expected\norbit",
                              "Placed satellites into safe mode",
                              "Notified customers/stakeholders of coming\nevent and potential service disruption",
                              "Postponed launch activities for new\nsatellites",
                              "Canceled routine maintenance of network\n(e.g., of terrestrial assets)",
                              "Canceled routine maintenance of network\nto free up maintenance teams",
                              "Placed staff teams on alert to implement\nexisting preparation plans",
                              "Canceled employee time off",
                              "Increased staff situational awareness of\nthe incoming space weather event"
                              #"Please add any other considerations we did not list:"#
                            )
)

data$employees = factor(data$employees,
                        levels=c(
                          "1-24",
                          "25-99",
                          "100-199",
                          "200-499",
                          "500+",
                          ""
                        ),
                        labels=c(
                          "1-24",
                          "25-99",
                          "100-199",
                          "200-499",
                          ">500",
                          "Not Disclosed"
                        )
)

#get structure 
str(data)

#get columns
subset = data %>% select(
  Response_Type, Response_Value,
  employees
)
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

# Summarise the data
summarised_data <- subset %>%
  group_by(Response_Type, employees) %>%
  summarise(Response_Value = sum(Response_Value, na.rm = TRUE), .groups = "drop")

# Summarize the data to get total sum per Response_Type
labels <- summarised_data %>%
  group_by(Response_Type) %>%
  summarise(group_sum = sum(Response_Value, na.rm = TRUE)) %>%
  ungroup()

summarised_data <- summarised_data %>%
  complete(Response_Type, employees, fill = list(Response_Value = 0))

original_levels <- c("1-24", "25-99", "100-199", "200-499", ">500")#, "Not Disclosed")
summarised_data$employees <- factor(summarised_data$employees, levels = rev(original_levels))
summarised_data = summarised_data[complete.cases(summarised_data),]

# plot data
plot3 = ggplot(summarised_data, 
              aes(x = factor(Response_Type, levels = rev(unique(Response_Type))), 
                  y = Response_Value)) +
  geom_bar(aes(fill = employees), stat = "identity") + 
  geom_text(data = labels, aes(x = Response_Type, y = group_sum, label = group_sum), 
            vjust = .4, hjust = -1, size = 4) + 
  coord_flip() +
  guides(fill = guide_legend(nrow = 1, ncol = 10, reverse = TRUE)) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    legend.box.just = "center"
  ) +
  labs(colour=NULL,
       title = "Satellite: During-Event Decisions Taken",
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
data <- read.csv(file.path(folder, 'data', 'survey_responses', filename))

##tells us the structure, including column names
str(data)

# subset columns we want
data <- data %>% select(
  "response_id",
  "post.event.section..Assessed.Joule.heating.impacts.",
  "post.event.section..Estimated.total.ionizing.dose.",
  "post.event.section..Estimated.displacement.damage.dose.",
  "post.event.section..Physically.replaced.exposed.satellites.with.new.assets....1m.in.value..",
  "post.event.section..Physically.replaced.exposed.satellites.with.new.assets...1.10m.in.value..",
  "post.event.section..Physically.replaced.exposed.satellites.with.new.assets...10.100m.in.value..",
  "post.event.section..Physically.replaced.exposed.satellites.with.new.assets....100m.in.value..",
  "post.event.section..Placed.an.order.to.replace.exposed.assets....1m.in.value..",
  "post.event.section..Placed.an.order.to.replace.exposed.assets...1.10m.in.value..",
  "post.event.section..Placed.an.order.to.replace.exposed.assets...10.100m.in.value..",
  "post.event.section..Placed.an.order.to.replace.exposed.assets....100m.in.value..",
  "post.event.section..Liaised.with.other.operators.to.share.data.",
  "post.event.section..Liaised.with.other.operators.to.share.experiences.",
  "post.event.section..Revised.preparation.plans.for.future.events.",
  "post.event.section..Undertaken.new.modeling.and.simulation.activities."
  #"post.event.section..Please.add.any.other.considerations.we.did.not.list."#
)

data_long <- data %>%
  pivot_longer(cols = -c(response_id),
               names_to = "Response_Type",
               values_to = "Response_Value")

folder <- dirname(rstudioapi::getSourceEditorContext()$path)
data <- read.csv(file.path(folder, 'data', 'survey_responses', filename))

colnames(data)[colnames(data) == "survey.section..How.many.employees.does.your.company.have."] <- "employees"
colnames(data)[colnames(data) == "survey.section..How.many.customers.does.your.organization.have."] <- "customers"
colnames(data)[colnames(data) == "survey.section..How.many.primary.satellites.do.you.operate."] <- "primary_satellites"
colnames(data)[colnames(data) == "survey.section..What.is.the.primary.service.you.offer.support."] <- "primary_service"
colnames(data)[colnames(data) == "survey.section..Which.region.s..do.you.operate.in."] <- "region"
colnames(data)[colnames(data) == "survey.section..Which.country.region.is.your.HQ.located."] <- "country"


str(data)

# subset columns we want
data <- data %>% select(
  "response_id",
  "employees",
  "customers",
  "primary_satellites",
  "primary_service",
  "region",
  "country"
)

#merge long dataframe with company characteristics
data <- merge(data_long, data, by = "response_id")

#remove old df
rm(data_long)

data$Response_Type = factor(data$Response_Type,
                            levels=c(
                              "post.event.section..Assessed.Joule.heating.impacts.",
                              "post.event.section..Estimated.total.ionizing.dose.",
                              "post.event.section..Estimated.displacement.damage.dose.",
                              "post.event.section..Physically.replaced.exposed.satellites.with.new.assets....1m.in.value..",
                              "post.event.section..Physically.replaced.exposed.satellites.with.new.assets...1.10m.in.value..",
                              "post.event.section..Physically.replaced.exposed.satellites.with.new.assets...10.100m.in.value..",
                              "post.event.section..Physically.replaced.exposed.satellites.with.new.assets....100m.in.value..",
                              "post.event.section..Placed.an.order.to.replace.exposed.assets....1m.in.value..",
                              "post.event.section..Placed.an.order.to.replace.exposed.assets...1.10m.in.value..",
                              "post.event.section..Placed.an.order.to.replace.exposed.assets...10.100m.in.value..",
                              "post.event.section..Placed.an.order.to.replace.exposed.assets....100m.in.value..",
                              "post.event.section..Liaised.with.other.operators.to.share.data.",
                              "post.event.section..Liaised.with.other.operators.to.share.experiences.",
                              "post.event.section..Revised.preparation.plans.for.future.events.",
                              "post.event.section..Undertaken.new.modeling.and.simulation.activities."
                              #"post.event.section..Please.add.any.other.considerations.we.did.not.list."#
                            ),
                            labels=c(
                              "Assessed Joule heating impacts",
                              "Estimated total ionizing dose",
                              "Estimated displacement damage dose",
                              "Physically replaced exposed satellites\nwith new assets (<$1m in value)",
                              "Physically replaced exposed satellites\nwith new assets ($1-10m in value)",
                              "Physically replaced exposed satellites\nwith new assets ($10-100m in value)",
                              "Physically replaced exposed satellites\nwith new assets (>$100m in value)",
                              "Placed an order to replace exposed assets\n(<$1m in value)",
                              "Placed an order to replace exposed assets\n($1-10m in value)",
                              "Placed an order to replace exposed assets\n($10-100m in value)",
                              "Placed an order to replace exposed assets\n(>$100m in value)",
                              "Liaised with other operators to share data",
                              "Liaised with other operators to share\nexperiences",
                              "Revised preparation plans for future events",
                              "Undertaken new modeling and simulation\nactivities"
                              #"Please add any other considerations we did not list:"#
                            )
)

data$employees = factor(data$employees,
                        levels=c(
                          "1-24",
                          "25-99",
                          "100-199",
                          "200-499",
                          "500+",
                          ""
                        ),
                        labels=c(
                          "1-24",
                          "25-99",
                          "100-199",
                          "200-499",
                          ">500",
                          "Not Disclosed"
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

# Summarize the data to get total sum per Response_Type
labels <- summarised_data %>%
  group_by(Response_Type) %>%
  summarise(group_sum = sum(Response_Value, na.rm = TRUE)) %>%
  ungroup()

summarised_data <- summarised_data %>%
  complete(Response_Type, employees, fill = list(Response_Value = 0))

original_levels <- c("1-24", "25-99", "100-199", "200-499", ">500")#, "Not Disclosed")
summarised_data$employees <- factor(summarised_data$employees, levels = rev(original_levels))
summarised_data = summarised_data[complete.cases(summarised_data),]

# plot data
plot4 = ggplot(summarised_data, 
               aes(x = factor(Response_Type, levels = rev(unique(Response_Type))), 
                   y = Response_Value)) +
  geom_bar(aes(fill = employees), stat = "identity") + 
  geom_text(data = labels, aes(x = Response_Type, y = group_sum, label = group_sum), 
            vjust = .4, hjust = -1, size = 4)+ 
  coord_flip() +
  guides(fill = guide_legend(nrow = 1, ncol = 10, reverse = TRUE)) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    legend.box.just = "center"
  ) +
  labs(colour=NULL,
       title = "Satellite: Post-Event Decisions Taken",
       subtitle = "Decisions taken once the event is over.",
       x = NULL, y = "Responses",
       fill="Employees") +
  theme(panel.spacing = unit(0.6, "lines")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)),
                     limits=c(0,max_value)) +
  scale_fill_viridis_d(direction=1) 

##################
### Incidents
##################
# Set default folder
folder <- dirname(rstudioapi::getSourceEditorContext()$path)
data <- read.csv(file.path(folder, 'data', 'survey_responses', filename))

# ##tells us the structure, including column names
# str(data)

# subset columns we want
data <- data %>% select(
  "response_id",
  "incident.event.section..Interruption.of.satellite.services.to.customers.",
  "incident.event.section..Satellite.asset.failure.",
  "incident.event.section..Temporary.loss.of.satellite.communication.uplink.downlink.connections.",
  "incident.event.section..Degraded.photovoltaic.cell.efficiency.",
  "incident.event.section..Temporary.loss.of.onboard.systems.",
  "incident.event.section..Permanent.loss.of.onboard.systems."
  #"incident.event.section..Please.add.any.other.considerations.we.did.not.list."#
)

data_long <- data %>%
  pivot_longer(cols = -c(response_id),
               names_to = "Response_Type",
               values_to = "Response_Value")

folder <- dirname(rstudioapi::getSourceEditorContext()$path)
data <- read.csv(file.path(folder, 'data', 'survey_responses', filename))

colnames(data)[colnames(data) == "survey.section..How.many.employees.does.your.company.have."] <- "employees"
colnames(data)[colnames(data) == "survey.section..How.many.customers.does.your.organization.have."] <- "customers"
colnames(data)[colnames(data) == "survey.section..How.many.primary.satellites.do.you.operate."] <- "primary_satellites"
colnames(data)[colnames(data) == "survey.section..What.is.the.primary.service.you.offer.support."] <- "primary_service"
colnames(data)[colnames(data) == "survey.section..Which.region.s..do.you.operate.in."] <- "region"
colnames(data)[colnames(data) == "survey.section..Which.country.region.is.your.HQ.located."] <- "country"

# str(data)

# subset columns we want
data <- data %>% select(
  "response_id",
  "employees",
  "customers",
  "primary_satellites",
  "primary_service",
  "region",
  "country"
)

#merge long dataframe with company characteristics
data <- merge(data_long, data, by = "response_id")

#remove old df
rm(data_long)

data$Response_Type = factor(data$Response_Type,
                            levels=c(
                              "incident.event.section..Interruption.of.satellite.services.to.customers.",
                              "incident.event.section..Satellite.asset.failure.",
                              "incident.event.section..Temporary.loss.of.satellite.communication.uplink.downlink.connections.",
                              "incident.event.section..Degraded.photovoltaic.cell.efficiency.",
                              "incident.event.section..Temporary.loss.of.onboard.systems.",
                              "incident.event.section..Permanent.loss.of.onboard.systems."
                              #"incident.event.section..Please.add.any.other.considerations.we.did.not.list."#
                            ),
                            labels=c(
                              "Interruption of satellite\nservices to customers",
                              "Satellite asset failure",
                              "Temporary loss of satellite communication\nuplink/downlink connections",
                              "Degraded photovoltaic\ncell efficiency",
                              "Temporary loss of\nonboard systems",
                              "Permanent loss of\nonboard systems"
                              #"Please add any other considerations we did not list:"#
                            )
)

data$employees = factor(data$employees,
                        levels=c(
                          "1-24",
                          "25-99",
                          "100-199",
                          "200-499",
                          "500+",
                          ""
                        ),
                        labels=c(
                          "1-24",
                          "25-99",
                          "100-199",
                          "200-499",
                          ">500",
                          "Not Disclosed"
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

summarised_data <- summarised_data %>%
  complete(Response_Type, employees, fill = list(Response_Value = 0))

original_levels <- c("1-24", "25-99", "100-199", "200-499", ">500")#, "Not Disclosed")
summarised_data$employees <- factor(summarised_data$employees, levels = rev(original_levels))
summarised_data = summarised_data[complete.cases(summarised_data),]

# plot data
plot5 = ggplot(summarised_data, 
               aes(x = Response_Type, y = Response_Value, fill = employees)) +
  geom_bar(stat = "identity") + 
  coord_flip() +
  theme_minimal() +
  theme(legend.position = 'bottom') +
  labs(colour=NULL,
       title = "(E) Event Incidents",
       subtitle = "These are decisions taken after a space weather forecast has been issued.",
       x = NULL, y = "Responses",
       fill="Employees") +
  theme(panel.spacing = unit(0.6, "lines")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)),
                     limits=c(0, max_value)) +
  scale_fill_viridis_d(direction=1) 

panel <- ggarrange(plot1, plot2, plot3, plot4, #plot5,# plot6,
                   ncol = 2, nrow = 2, #align = c("hv"),
                   common.legend = TRUE,
                   legend='bottom'#,
                   # heights=c(1,.95)
)

ggsave(
  'satellite_panel.png',
  plot = last_plot(),
  device = "png",
  path=folder_figs, 
  units = c("in"),
  width = 13,
  height = 10,
  bg="white"
)

ggsave(
  'satellite_1_long_term.png',
  plot = plot1,
  device = "png",
  path=folder_figs,
  units = c("in"),
  width = 8,
  height = 6,
  bg="white"
)

ggsave(
  'satellite_2_short_term.png',
  plot = plot2,
  device = "png",
  path=folder_figs,
  units = c("in"),
  width = 8,
  height = 6,
  bg="white"
)

ggsave(
  'satellite_3_during_event.png',
  plot = plot3,
  device = "png",
  path=folder_figs,
  units = c("in"),
  width = 8,
  height = 6,
  bg="white"
)

ggsave(
  'satellite_4_post_event.png',
  plot = plot4,
  device = "png",
  path=folder_figs,
  units = c("in"),
  width = 8,
  height = 6,
  bg="white"
)

