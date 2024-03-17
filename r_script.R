library(ggplot2)
df <- read.csv('bio_data.csv', sep = ';')
df$seed <- factor(df$seed)
df$protector_type <- factor(df$protector_type)
df$is_bathed <- as.logical(df$is_bathed)
df$is_freezed <- as.logical(df$is_freezed)

#Выбор растения
#current_seed <- 'шалфей'
current_seed <- 'змееголовник'

df_seed <- subset(df, seed == current_seed)

#Разделение контроля и эксперимента
is_control_freezed <- F
exp <- subset(df_seed, protector_type != 'контроль')
control <- subset(df_seed, protector_type == 'контроль' & is_freezed == is_control_freezed)

#Определение средних контрольных значений
energy_c <- mean(control$energy)
germination_c <- mean(control$germination)

###
#Графики
###

#Анализ всех протекторов
agg <- aggregate(
  cbind(energy / energy_c, germination / germination_c)
  ~ protector_type + protector_dose + is_bathed, 
  data = exp, 
  mean)
colnames(agg)[4:5] <- c('energy', 'germination')

plot_agg <- ggplot(agg, aes(x = energy, y = germination, col = protector_type, shape = is_bathed, size = protector_dose))+
  geom_point()+
  #theme(legend.position = 'bottom')+
  labs(x = 'Энергия', y = 'Всхожесть', color = 'Тип криопротектора', shape = 'Баня', size = 'Доза криопротектора')
  ggtitle(paste('Диаграмма всхожести и энергии, ', current_seed, sep = ''))
img_name <- paste('images/', current_seed, '/общая_диаграмма', ifelse(is_control_freezed, '(замороженный_контроль)', ''), '.png', sep = '')
ggsave(
    img_name,
    plot = plot_agg)

#Анализ каждого протектора по отдельности

for (current_protector in unique(exp$protector_type)) {
  #Выборка из эксперимента по текущему протектору
  exp_prot <- subset(exp, protector_type == current_protector)
  agg_prot <- aggregate(
    cbind(energy / energy_c, germination / germination_c)
    ~ protector_type + protector_dose + is_bathed, 
    data = exp_prot, 
    mean
  )
  
  #Выборка по протектору с баней
  exp_prot_b <- subset(exp, protector_type == current_protector & is_bathed == T)
  agg_prot_b <- aggregate(
    cbind(energy / energy_c, germination / germination_c)
    ~ protector_type + protector_dose + is_bathed, 
    data = exp_prot_b, 
    mean
  )
  colnames(agg_prot_b)[4:5] <- c('energy', 'germination')
  
  #Выборка по протектору без бани
  exp_prot_nb <- subset(exp, protector_type == current_protector & is_bathed == F)
  agg_prot_nb <- aggregate(
    cbind(energy / energy_c, germination / germination_c)
    ~ protector_type + protector_dose + is_bathed, 
    data = exp_prot_nb, 
    mean
  )
  colnames(agg_prot_nb)[4:5] <- c('energy', 'germination')
  
  #График энергии
  plot_energy <- ggplot()+
    geom_smooth(data = agg_prot_b, aes(x = protector_dose, y = energy, color = 'С баней'))+
    geom_smooth(data = agg_prot_nb, aes(x = protector_dose, y = energy, color = 'Без бани'))+
    xlab("Доза криопротектора")+
    ylab("Энергия")+
    ggtitle(current_protector)+
    scale_color_manual('Легенда', values = c('С баней' = 'red', 'Без бани' = 'blue'))
  img_name <- paste('images/', current_seed, '/энергия_', current_protector, ifelse(is_control_freezed, '(замороженный_контроль)', ''), '.png', sep = '')
  ggsave(
    img_name,
    plot = plot_energy)
  
  #График всхожести
  plot_germination <- ggplot()+
    geom_smooth(data = agg_prot_b, aes(x = protector_dose, y = germination, color = 'С баней'))+
    geom_smooth(data = agg_prot_nb, aes(x = protector_dose, y = germination, color = 'Без бани'))+
    xlab("Доза криопротектора")+
    ylab("Всхожесть")+
    ggtitle(current_protector)+
    scale_color_manual('Легенда', values = c('С баней' = 'red', 'Без бани' = 'blue'))
  img_name <- paste('images/', current_seed, '/всхожесть_', current_protector, ifelse(is_control_freezed, '(замороженный_контроль)', ''), '.png', sep = '')
  ggsave(
    img_name,
    plot = plot_germination)
}






