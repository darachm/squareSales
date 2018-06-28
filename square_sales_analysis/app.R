library(tidyverse)
library(shiny)
library(lubridate)

ui <- fluidPage(
    titlePanel("h"),
    mainPanel(
        fileInput(inputId="input_csv", 
            label="Give me a square space export CSV",
            multiple=T,
            accept=c("csv","text/csv"),
            placeholder="placeholder"
            ),
        textOutput("files_read",inline=F),
        textOutput("files_cols",inline=F)
        )
    )

dollars_to_numbers <- function(x) as.numeric(sub("\\$","",x))

tz_lookup <- c(`Pacific Time (US & Canada)`="America/Los_Angeles")

parse_square_csv <- function(path) {
    read_csv(path) %>% 
        mutate_at(.,
            .vars=c("Gross Sales","Discounts","Net Sales","Tax"),
            .funs=dollars_to_numbers) %>%
        mutate(.,   DateString=str_c(Date,"_",Time)) %>%
        group_by(., DateString,`Time Zone`) %>%
        mutate(.,DateTime=parse_date_time(DateString,"m/d/y_h:M:S",
            tz=tz_lookup[unique(`Time Zone`)])) %>%
        ungroup(.) %>% 
        mutate(.,
            DateTime=with_tz(DateTime,
                tzone="America/Los_Angeles")
            ) %>%
        select(.,-DateString,-Date,-Time,-SKU,-Details) %>%
        mutate(.,DayOfWeek=   wday(DateTime))%>%
        mutate(.,DayOfYear=   yday(DateTime))%>%
        mutate(.,Week=        week(DateTime)) %>% 
        filter(.,-Discounts == `Gross Sales`) %>%
        arrange(.,DateTime)
}
z <- parse_square_csv("../copy.csv")

# Internal check for any discounts greater than the sales, there's 
# none on the test dataset
# z%>% filter(Discounts != -`Gross Sales`, -Discounts > `Gross Sales`) %>% thead

z %>% group_by(.,DayOfWeek) %>% 
    summarize(.,Transactions=list(table(Item)))

ggplot(.) +
    aes(x=DayOfWeek,y=`Gross Sales`) + 
    geom_point() +
    NULL

z %>% 
    ggplot(.) +
    aes(x=DayOfWeek,fill=Item) + 
    geom_bar() +
    NULL

# to sort the table by descending size of items
#    {tst <- sort(table(.$Item),decreasing=T); 
#        .$Order <- sapply(.$Item,function(x) which(names(tst)==x)); 
#        return(arrange(.,Order))} %>% 

server <- function(input, output) {
    dataframe_list <- reactive({
        datapath <- input$input_csv$datapath
        if (is.null(datapath)) {
            return(NULL)
        }
        return(bind_rows(lapply(datapath,read_csv)))
        })
    output$files_read <- reactive({
        name_vector <- input$input_csv$name
        if (is.null(name_vector)) {
            return("")
        }
        name_list <- str_c(input$input_csv$name,collapse=", ")
        return(str_c("I'm reading in these files : ",name_list))
        })
    output$files_cols <- reactive({
        datar <- dataframe_list()
        if (is.null(datar)) {
            return("")
        }
        name_list <- str_c(names(dataframe_list()),collapse=", ")
        return(str_c("I stuck them all together, and they have ",
            "these columns : ",
            name_list))
        })
    output$report <- reactive({
        })

#So to give you some context - currently we use an online sales system (SQUARE) which is our register and sales tracker. They allow us to export our sales data. An example of last week's sales are attached. To start, it might help to focus on LUNCH. Some context about how our lunch menu is organized: it is set-up in a way where you choose (2) things: (1) the type of dish (2) the type of protein
#
#        Menu Items (Item)
#            Bánh Mi
#            Rice Bowl
#            Noodle Bowl
#            Pho Soup
#            Cold Soba Salad
#            Papaya Salad
#            Kale Salad
#            Peanut Salad
#            Spring Rolls 
#        Protein Options (price point name)
#            Curry Chicken
#            Grilled Chicken
#            Coconut-Milk Fried Chicken
#            Slow cooked Pork
#            Grilled pork
#            Thai basil tofu
#            Chili Garlic Tofu
#            Roasted Cauliflower 
#
#A really straightforward report would help us see how much to prepare in the kitchen each day so we have MINIMUM WASTE. Part of the difficulty in running a cafe is that we want to find a sweet spot where we make enough to maximize sales but minimize waste (extra food). 
#
#Specifically, how can we generate a report that would clearly show (2) simple totals:
#
#    How much of each protein option we sell each day
#    How much of each item we sell each day 
#
#Let me know what you think. There are currently report tools within Square and through a 3rd party app called DASHBOARD that has been helpful. If you'd like to see these, I can call you to share our login info. 

#
#bbmSales %>% thead
#
#bbmSales %>% filter(NetSales==0) %>% thead
#
#bbmSales <- bbmSales %>% filter(NetSales!=0)
#
#g <- bbmSales %>% ggplot()+theme_classic()+
#  aes(x=DateTime,weight=NetSales,col=Category)+
#  geom_line(stat="bin",binwidth=int_length(300))+
#  facet_wrap(~factor(str_c(DayOfYear,", weekday ",DayOfWeek)),
#    scales="free",ncol=5)+
#  ylab("NetSales")+xlab("5 min periods")
#g
#
#ggsave("salesThroughDay.png",g,width=10,height=07)
#
#
#byItem <- bbmSales %>% group_by(Category,Item) %>%
#  summarize(SumNetSales=sum(NetSales),SumQty=sum(Qty)) %>% 
#  arrange(-SumNetSales)
#
#write_tsv(byItem,path="bbm_byItem.tsv")
#
#byItemPP <- bbmSales %>% group_by(Category,Item,`Price Point Name`) %>%
#  summarize(SumNetSales=sum(NetSales),SumQty=sum(Qty)) %>% 
#  arrange(-SumNetSales)
#
#write_tsv(byItemPP,path="bbm_byItemPP.tsv")
#
#byItemPPByWeekday <- bbmSales %>% 
#  group_by(Category,Item,`Price Point Name`,DayOfWeek) %>%
#  summarize(SumNetSales=sum(NetSales),SumQty=sum(Qty)) %>% 
#  arrange(-SumNetSales)
#
#write_tsv(byItemPPByWeekday,path="bbm_byItemPPByWeekday.tsv")

}

shinyApp(ui = ui, server = server)
