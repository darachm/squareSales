library(tidyverse)
library(shiny)
library(lubridate)


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
        select(.,-DateString,-Date,-Time,-SKU,-Details,
            -`Net Sales`
            ) %>%
        mutate(.,DayOfWeek=   wday(DateTime,label=T))%>%
        mutate(.,DayOfYear=   yday(DateTime))%>%
        mutate(.,Week=        week(DateTime)) %>% 
        filter(.,-Discounts == `Gross Sales`) %>%
        mutate(.,`Price Point Name`=ifelse(is.na(`Price Point Name`),
            "",`Price Point Name`)) %>%
        arrange(.,DateTime)
}

# Internal check for any discounts greater than the sales, there's 
# none on the test dataset
# z%>% filter(Discounts != -`Gross Sales`, -Discounts > `Gross Sales`) %>% thead

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
        textOutput("files_cols",inline=F),
        radioButtons(inputId="items_or_sales",
            label="Items or sales?",
            choices=list(`Item counts`="Item",`Net Sales`="Net Sales")
            ),
        checkboxGroupInput(inputId="group_by",
            label="Group by?",
            choices=list("Item","Price Point Name")
            ),
        tableOutput(outputId="summary_table"),
        plotOutput( outputId="summary_plot")
        )
    )

#    How much of each protein option we sell each day
#    How much of each item we sell each day 

z <- parse_square_csv("../copy.csv")

server <- function(input, output) {
    datar <- reactive({
        datapath <- input$input_csv$datapath
        if (is.null(datapath)) {
            return(NULL)
        }
        return(bind_rows(lapply(datapath,parse_square_csv)))
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
        datar <- datar()
        if (is.null(datar)) {
            return("")
        }
        name_list <- str_c(names(datar),collapse=", ")
        return(str_c("I stuck them all together, and they have ",
            "these columns : ",
            name_list))
        })
    output$debug <- renderText(is(input$items_or_sales))
    output$debug <- renderText(is(input$group_by))

    output$summary_table <- reactive({
        datar <- datar()
        datar <- z
        return(datar)
        if (is.null(datar)) {
            return("")
        }
        datar() %>% group_by_at(.,.vars=input$group_by) %>%
            summarize(.,SummaryStat=!!!quos(input$items_or_sales)) #%>%
#            summarize(.,SummaryStat=length(Item)) %>%
#            select(.,Item,SummaryStat)
#            select_at(.,!!!quos(input$group_by)) 
        })
    output$summary_plot <- reactive({
        datar <- datar() 
        if (is.null(datar)) {
            return(datar)
        }
        facet_by <- c("Item")
        color_by <- c("Price Point Name")
        x_axis <- c("DayOfWeek")
        groupers <- c(facet_by, color_by, x_axis)
        for (i in groupers) {
            datar[str_c(i,"_ranked")] <- order(datar[i][[1]],decreasing=T)
        }
        datar %>% arrange_(.,str_c(groupers,"_ranked")) %>% 
            mutate_at(.,.vars=groupers,factor) %>%
            group_by_at(.,.vars=groupers) %>%
            summarize(.,TotalSales=sum(`Gross Sales`),
                TotalSold=length(`Gross Sales`)) %>%
            gather(.,Variable,Value,-groupers) %>%
            ggplot(.) +
            aes_string(x=x_axis,y=quote(Value),
                fill=str_c(str_replace(color_by,"(.*)","\\`\\1\\`"),collapse=":"))+
            geom_bar(stat="identity")+
            facet_wrap(facets=c("Variable",facet_by),scales="free_y")+
            guides(fill=F)+
            NULL
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

#z %>% group_by(.,DayOfWeek) %>% 
#    summarize(.,Transactions=list(table(Item)))
#z %>% ggplot(.) +
#    aes(x=factor(DayOfWeek),fill=Item) + 
#    geom_bar(position="stack") +
#    NULL

}

shinyApp(ui = ui, server = server)
