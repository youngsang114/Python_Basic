from bs4 import BeautifulSoup as bs
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
import pandas as pd
from datetime import datetime

path="C:\\Users\\dgh06\\OneDrive\\문서\\GitHub\\python-work\\chromedriver.exe"

driver = webdriver.Chrome(path)
code = '005380'

driver.get('https://navercomp.wisereport.co.kr/v2/company/c1010001.aspx?cmp_cd='+code)

element = driver.find_element(By.CLASS_NAME,'menu')
print(element)

element2=element.find_elements(By.TAG_NAME,'a')[3]
element2.click()

soup = bs(driver.page_source,"html.parser")
data=soup.find_all('table',attrs={
    'class':"gHead01 all-width data-list"})[1]

# 컬럼 부분에 들어갈 값을 추춤
_list=data.find_all('th')
col_list = []
for i in _list:
    col_list.append(i.get_text())

col=[]
for i in col_list:
    result=(i.replace('/n',""))
    result=result.replace("연간컨센서스보기","").replace("연간컨센서스닫기","")
    col.append(result.strip())
    result=result.replace(u"\xa0",u"")

# value 값을 추출
data2=data.find('tbody')
data3=data2.find_all('tr')
value_list=[]

for i in data3:
    sample_list=[]
    _list=i.find_all('td')
    for j in _list:
        sample_list.append(j.get_text())
    value_list.append(sample_list)
value_list

DF=pd.DataFrame(value_list,columns=col)

def change(x):
    result=x.replace("\n","")
    result=result.replace("\t","")
    result=result.replace("펼치기","")
    result=result.replace(u"\xa0",u"")
    
    return result
DF["항목"]=DF["항목"].apply(change)

DF.columns = ['항목', '2018/12(IFRS연결)', '2019/12(IFRS연결)', '2020/12(IFRS연결)',
       '2021/12(IFRS연결)', '2022/12(IFRS연결)', '2023/12(E)(IFRS연결)', '전년대비(2022)',
       '전년대비(2023)']

now=datetime.now()
now=now.strftime("%y%m%d%H%M%S")
#print(str(now))

DF.to_csv(f"C:/Users/dgh06/OneDrive/문서/GitHub/python-work/230411/test({now}).csv",index=False)