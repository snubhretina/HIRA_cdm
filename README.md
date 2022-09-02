# 건강보험심사 평가원 CDM
- 원내 CDM을 이용한 R code 업로드 예정

### [1] 원내 CDM Cox모형

| ATLAS 사용하지 않고 R 코드로 코호트 생성

### Cohort

1. Index Date 
    - **Practice**(원내 CDM) : 2016년 1월 1일
    - **Original**(심사평가원 CDM): 2018년 1월 1일
2. COVID19를 이상지질혈증(dyslipidemia)으로 대체 
    
    
    |  | condition |
    | --- | --- |
    | dyslipidemia  | 4159131(descendant 포함) |
    | RVO  | 312622, 313761, 4334247  (descendant  포함) → 설정 필요  |
    | CSC | 설정 필요  |
    - 2018년 1월 1일 이후로 이상지질혈증 진단 환자 : 22381명
    - 2016년 1월 부터 2020년 까지 RVO발생 환자 : 166명
    - 2018년 1월 1일 후 RVO 발생환자 : 91명
    
    |  | dyslipidemia | RVO |
    | --- | --- | --- |
    | 2016년 ~ 2017년 12월 | X | O or X |
    | 2018년 1월 ~ 2020년 12월 | O | O or X |
3. Original (심사평가원 CDM)
    
    
    |  | condition | observation  |
    | --- | --- | --- |
    | COVID-19 | 37311061, 439676, 4100065 | 704996 |
    | RVO  | 312622, 313761, 4334247  (descendant  포함) → 설정 필요 |  |
    | CSC | 설정 필요 |  |
    
    |  | Covid19 | RVO |
    | --- | --- | --- |
    | 2018년 ~ 2019년 12월 | X | O or X |
    | 2020년 1월 ~ 2022년 4월 | O | O or X |
4. End Date
    - **Practice**(원내 CDM) : 2020년 ****12월 31일
    - **Original**(심사평가원 CDM): 2022년 4월 30일

### Analysis

> Time-varying covariate Cox모형을 위한 데이터 전처리 단계 중
> 

환자 당

- time-varying covariate(dyslipidemia)를 기간별로 0 or 1 로 나누기
- Outcome event(RVO)를 기간별로 0 or 1로 나누기
- 결과 값
    - K-M plot / Survival Table / Cox 모형 진단 및 결과
    

---

### [2] 원내 CDM ITS 분석

### 1. 08.26

### Cohort

- **Concept ID 설정 필요**
    1. 전체 vitrectomy 뽑기- 유리체절제술 ————————————————————— O
    2. 전체 valve surgery, trabeculectomy 뽑기 
    (녹내장 수술(안압을 낮추는 것이 주 목적) / 방수유출장치(녹내장 임플란트) 삽입술 / 섬유주 절제술)
    3. 전체 LI 뽑기
    4. 진단명 별 분류
        1. RRD (OR including RD)
        2. Macular Hole (H35.33)
        3. Glaucoma

### Analysis

- 수술례 및 주사례를 모으는 연구
- 결과 값 : 빈도수 plot + time period (event 시점 찾기)
- 빈도수가 줄어드는 시점
    - 대구코로나
    - 신천지 코로나
    - 거리두기 격상 또는 완화
