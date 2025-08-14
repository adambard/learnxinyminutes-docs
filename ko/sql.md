---
name: SQL
filename: learnsql.sql
contributors:
  - ["Bob DuCharme", "http://bobdc.com/"]
  - ["Th3G33k", "https://github.com/Th3G33k"]
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

구조적 쿼리 언어(SQL)는 테이블 집합에 저장된 데이터베이스를 만들고 작업하기 위한 [ISO/IEC 9075](https://www.iso.org/standard/63555.html) 표준 언어입니다. 구현은 일반적으로 언어에 자체 확장을 추가합니다. [다양한 SQL 구현 비교](http://troels.arvin.dk/db/rdbms/)는 제품 차이점에 대한 좋은 참조 자료입니다.

구현은 일반적으로 여기에 표시된 명령을 대화식으로 입력할 수 있는 명령줄 프롬프트를 제공하며, 스크립트 파일에 저장된 일련의 명령을 실행하는 방법도 제공합니다. (대화형 프롬프트가 끝났음을 보여주는 것은 표준화되지 않은 좋은 예입니다. 대부분의 SQL 구현은 QUIT, EXIT 또는 둘 다 키워드를 지원합니다.)

이러한 샘플 명령 중 일부는 [GitHub](https://github.com/datacharmer/test_db)에서 사용할 수 있는 [MySQL 직원 샘플 데이터베이스](https://dev.mysql.com/doc/employee/en/)가 이미 로드되었다고 가정합니다. GitHub 파일은 가상 회사의 직원에 대한 데이터 테이블을 만들고 채우는 아래 관련 명령과 유사한 명령 스크립트입니다. 이러한 스크립트를 실행하는 구문은 사용 중인 SQL 구현에 따라 다릅니다. 운영 체제 프롬프트에서 실행하는 유틸리티가 일반적입니다.


```sql
-- 주석은 두 개의 하이픈으로 시작합니다. 각 명령은 세미콜론으로 끝납니다.

/*
여러 줄 주석
*/

-- SQL은 키워드에 대해 대소문자를 구분하지 않습니다. 여기 샘플 명령은
-- 데이터베이스, 테이블 및 열 이름과 구별하기 쉽도록
-- 대문자로 표기하는 관례를 따릅니다.

-- 데이터베이스 생성 및 삭제. 데이터베이스 및 테이블 이름은 대소문자를 구분합니다.
CREATE DATABASE someDatabase;
DROP DATABASE someDatabase;

-- 사용 가능한 데이터베이스 나열.
SHOW DATABASES;

-- 특정 기존 데이터베이스 사용.
USE employees;

-- 현재 데이터베이스의 departments 테이블에서 모든 행과 열 선택.
-- 기본 활동은 인터프리터가 화면에 결과를 스크롤하는 것입니다.
SELECT * FROM departments;

-- departments 테이블에서 모든 행을 검색하지만,
-- dept_no 및 dept_name 열만 검색합니다.
-- 명령을 여러 줄로 나누는 것은 괜찮습니다.
SELECT dept_no,
       dept_name FROM departments;

-- 모든 departments 열을 검색하지만 5개의 행만 검색합니다.
SELECT * FROM departments LIMIT 5;

-- departments 테이블에서 dept_name 값이 'en' 부분 문자열을
-- 포함하는 dept_name 열 값 검색.
SELECT dept_name FROM departments WHERE dept_name LIKE '%en%';

-- departments 테이블에서 dept_name 열이 'S'로 시작하고
-- 그 뒤에 정확히 4개의 문자가 있는 모든 열 검색.
SELECT * FROM departments WHERE dept_name LIKE 'S____';

-- titles 테이블에서 title 값을 선택하지만 중복은 표시하지 않습니다.
SELECT DISTINCT title FROM titles;

-- 위와 동일하지만 title 값으로 정렬됩니다(대소문자 구분).
-- 순서는 ASC(오름차순) 또는 DESC(내림차순)를 추가하여 지정할 수 있습니다.
-- 생략하면 기본적으로 오름차순으로 정렬됩니다.
SELECT DISTINCT title FROM titles ORDER BY title ASC;

-- 비교 연산자(=, >, <, >=, <=, <>) 및
-- 조건부 키워드(AND, OR)를 사용하여 쿼리를 구체화합니다.
SELECT * FROM departments WHERE dept_no = 'd001' OR dept_no = 'd002';

-- 위와 동일.
SELECT * FROM departments WHERE dept_no IN ('d001', 'd002');

-- 위의 반대.
SELECT * FROM departments WHERE dept_no NOT IN ('d001', 'd002');

-- 주어진 범위에서 선택.
SELECT * from departments WHERE dept_no BETWEEN 'd001' AND 'd002';

-- departments 테이블의 행 수 표시.
SELECT COUNT(*) FROM departments;

-- dept_name 값의 부분 문자열로 'en'을 포함하는
-- departments 테이블의 행 수 표시.
SELECT COUNT(*) FROM departments WHERE dept_name LIKE '%en%';

-- 집계 함수는 GROUP BY와 함께 사용하여 값 집합에서
-- 값을 계산할 수 있습니다. 가장 일반적으로 사용되는 함수는 다음과 같습니다:
-- MIN(), MAX(), COUNT(), SUM(), AVG().
-- HAVING을 사용하여 집계된 값으로 행을 필터링합니다.

-- 부서 번호별로 총 직원 수를 검색하며,
-- 직원 수가 100명 이상인 조건을 만족해야 합니다.
SELECT dept_no, COUNT(dept_no) FROM dept_emp GROUP BY dept_no
HAVING COUNT(dept_no) > 100;

-- 선택적 키워드 AS를 사용하는 별칭은 열/테이블 이름에 사용할 수 있습니다.
SELECT COUNT(A.*) AS total_employees, COUNT(B.*) total_departments
FROM employees AS A, departments B;

-- 일반적인 날짜 형식은 "yyyy-mm-dd"입니다.
-- 그러나 구현, 운영 체제 및 세션의 로캘에 따라 다를 수 있습니다.
SELECT * FROM dept_manager WHERE from_date >= '1990-01-01';

-- 여러 테이블의 정보 JOIN: titles 테이블은
-- 누가 어떤 직책을 가졌는지, 직원 번호별로,
-- 언제부터 언제까지인지 보여줍니다. 이 정보를 검색하지만,
-- 직원 번호 대신 직원 번호를 상호 참조로 사용하여
-- employees 테이블에서 각 직원의 이름과 성을
-- 가져옵니다. (그리고 10개의 행만 가져옵니다.)

SELECT employees.first_name, employees.last_name,
       titles.title, titles.from_date, titles.to_date
FROM titles INNER JOIN employees ON
       employees.emp_no = titles.emp_no LIMIT 10;

-- 여러 SELECT의 결과 결합.
-- UNION은 고유한 행을 선택하고, UNION ALL은 모든 행을 선택합니다.
SELECT * FROM departments WHERE dept_no = 'd001'
UNION
SELECT * FROM departments WHERE dept_no = 'd002';

-- SQL 구문 순서는 다음과 같습니다:
-- SELECT _ FROM _ JOIN _ ON _ WHERE _ GROUP BY _ HAVING _ ORDER BY _ UNION

-- 모든 데이터베이스의 모든 테이블 나열. 구현은 일반적으로
-- 현재 사용 중인 데이터베이스로 이 작업을 수행하는 자체 바로 가기 명령을 제공합니다.
SELECT * FROM INFORMATION_SCHEMA.TABLES
WHERE TABLE_TYPE='BASE TABLE';

-- 현재 사용 중인 데이터베이스에 대해 표시된 두 개의 열이 있는
-- tablename1이라는 테이블을 만듭니다. 데이터 유형과 같이
-- 열을 지정하는 방법에 대한 다른 많은 옵션이 있습니다.
CREATE TABLE tablename1 (fname VARCHAR(20), lname VARCHAR(20));

-- tablename1 테이블에 데이터 행 삽입. 이 테이블이
-- 이러한 값을 적절한 것으로 받아들이도록 정의되었다고 가정합니다.
INSERT INTO tablename1 VALUES('Richard','Mutt');

-- tablename1에서 lname 값이 'Mutt'인 모든 행에 대해
-- fname 값을 'John'으로 변경합니다.
UPDATE tablename1 SET fname='John' WHERE lname='Mutt';

-- tablename1 테이블에서 lname 값이 'M'으로 시작하는
-- 행 삭제.
DELETE FROM tablename1 WHERE lname LIKE 'M%';

-- tablename1 테이블에서 모든 행을 삭제하고 빈 테이블을 남깁니다.
DELETE FROM tablename1;

-- 전체 tablename1 테이블 제거.
DROP TABLE tablename1;
```

## 더 읽을거리

* [Codecademy - SQL](https://www.codecademy.com/learn/learn-sql) "하면서 배우기" 형식의 SQL에 대한 좋은 소개.
* [Database System Concepts](https://www.db-book.com) 책의 3장 - SQL 소개에는 SQL 개념에 대한 심층적인 설명이 있습니다.
