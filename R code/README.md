# CODE 설명(Settings & Functions)

## [1] 1_Settings

### 1-1. Library
-   필요한 패키지를 불러오고, 에러 시 install하는 code 
### 1-2. DataBase Connection
-   DB 연결 시 필요한 5가지 파라미터를 정의하는 부분(심평원)
    -   dbms
    -   user
    -   password
    -   server
    -   pathToDriver
-   DB 연결 하는 code

## [2] 2_Functions
| cdm_schema와 cdm_voca_schema 입력해야함(심평원)


### 2-1. Cohort Definition
| 세 개의 테이블에서 코호트를 만드는 함수들이 정의되어 있음

- return 값은 data에 대한 정보와 사용된 concept id들에 대한 정보를 list로 가지고 있음

- func 1. PROCEDURE TABLE(procedure_occurence)
  - procedureCohort: ancestor_id와 start_date를 인자로 전달할 수 있으며,ancestor_id는 필수인자, start_date는 선택인자로 입력하지 않아도 무방.
  - return value(list)
    - return$data 
      - person_id
      - procedure_concept_id
      - procedure_date
    - return$ids 
      - descendant_concept_id
      - ancestor_concept_id
      - concept_name
    
- func 2. CONDITION TABLE(condition_occcurence)
  - conditionCohort: ancestor_id와 start_date를 인자로 전달할 수 있으며, ancestor_id는 필수인자, start_date는 선택인자로 입력하지 않아도 무방.
  - return value(list)
    - return$data 
      - person_id
      - condition_concept_id
      - condition_start_date
    - return$ids 
      - descendant_concept_id
      - ancestor_concept_id
      - concept_name
      
-  func 3. DRUG EXPOSURE TABLE(drug_occurence)
  - conditionCohort: ancestor_id와 start_date를 인자로 전달할 수 있으며, ancestor_id는 필수인자, start_date는 선택인자로 입력하지 않아도 무방.
  - return value(list)
    - return$data 
      - person_id
      - drug_concept_id
      - drug_exposure_start_date
    - return$ids 
      - descendant_concept_id
      - ancestor_concept_id
      - concept_name
      
      
### 2-2. Make Data Frame
| func1~3에서 return 된 data에 대한 정보, concepst id들에 대한 정보들을 가지고 실질적인 테이블을 만들어 return하는 함수

- func 4(Procedure)
  - makeData_P & makeIdTable_P
- func 5(Condition)
  - makeData_C & makeIdTable_C
- func 6(Drug)
  - makeData_D & makeIdTable_D

- 2-2-1. makeData_(_)
  - _P, _C, _D  컬럼명만 다를뿐 동작은 동일
  - func 1~3 에서 return된 값들에서 $data로 접근해 우리가 아는 data.frame으로 만들어 return하는 함수 

- 2-2-2. makeIdTable_(_)
  - _P, _C, _D 컬럼명만 다를뿐 동작은 동일
  - func 1~3 에서 return된 값들에서 $ids로 접근.
  - makeData_(_)에서 return된  data.frame에서 사용된 concept_id들에 대한 정보(ancestor_id와 column_name)들을 data.frame으로 return 

### 2-3. Base Processing
| 여러 R 파일에서 자주 쓰이는 전처리 함수들 

- func 7. checkData
  - data를 인자로 받아 row의 개수와 사람 수를 return
  
- func 8. selectFirst
  - data를 인자로 받아 첫 병원 방문 row만 select해 data를 return
  
- func 9. unitCount
  - unit은 주(week), 월(month)같은 주 단위, 월 단위 이며. 주 별 합산을 집계 해주는 함수
