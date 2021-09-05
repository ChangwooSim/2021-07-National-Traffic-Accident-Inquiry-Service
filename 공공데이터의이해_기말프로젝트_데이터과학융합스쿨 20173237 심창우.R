# 패키지 라이브러리 불러오기
install.packages("httr")
library(httr)
library(shiny)
library(XML)

# 공공데이터 API 호출
service_url <- "http://apis.data.go.kr/B552061/lgStat/getRestLgStat?serviceKey="
service_key <- "0Hm6lssPI%2FQ0zqZB4o9Ol0IwrqpOKc4OOk1OKNjbNhb6xPOBoAlAs9cGywSHUv6aMKsJoEnQina0x1IUnecOcg%3D%3D"
service_addUrl <- "&type=xml&numOfRows=10&pageNo=1"

# UI 만들기
ui <- fluidPage(
  titlePanel("공공데이터의 이해 - 기말프로젝트"),
  sidebarLayout(
    sidebarPanel(
      selectInput("area", "지역 선택:",
                  c(서울="seoul", 경기 = "gyeonggido", 강원 = "gangwon")
      ),
      
      conditionalPanel(condition = "input.area == 'seoul'",
                       selectInput("loc1", "시군구 선택:",
                                   c("강남구"="1116", "강동구" = "1117", "노원구" = "1122",
                                     "송파구" = "1118", "중랑구" = "1121") 
                       )
      ),
      
      conditionalPanel(condition = "input.area == 'gyeonggido'",
                       selectInput("loc2", "시군구 선택:",
                                   c("구리시"="1310", "남양주시"="1334", "하남시"="1337", "가평군"="1322")
                       )
      ),
      
      conditionalPanel(condition = "input.area == 'gangwon'",
                       selectInput("loc3", "시군구 선택:",
                                   c("춘천시"="1401", "원주시"="1402", "양양군"="1423", "강릉시"="1404")
                       )
      ),
      
      dateInput("date", "연도 선택:", value = "2020-01-01")
    ),
    
    mainPanel(tableOutput("tab"))
  )
)


# server 만들기
server <- function(input, output) {
  
  output$tab <- renderTable({
    
    sido <- switch(input$area,
                   seoul = 1100,
                   gyeonggido = 1300,
                   gangwon = 1400)
    
    
    # 시군구 위치
    loc <- switch(input$area,
                  seoul = input$loc1,
                  gyeonggido = input$loc2,
                  gangwon = input$loc3)
    
    # 연도
    date <- format(input$date, "%Y")
    
    # API url
    url <- GET(paste0(service_url, service_key, '&searchYearCd=', date, '&siDo=', sido, '&guGun=', loc, service_addUrl))
    
    # XML 파싱
    doc <- xmlTreeParse(url, useInternalNodes = T, encoding = "UTF-8")
    rootNode <- xmlRoot(doc)
    items <- rootNode[[2]][['items']]
    
    df <- xmlToDataFrame(items)[,c("std_year","sido_sgg_nm", "acc_cl_nm","acc_cnt", "dth_dnv_cnt")]
  })
}

shinyApp(ui, server)
