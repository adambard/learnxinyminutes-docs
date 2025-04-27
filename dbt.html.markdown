---
language: dbt
filename: learndbt.sql
contributors:
  - ["isabel faulds", "https://github.com/manyshapes"]

---

Data Build Tool or dbt™ (core) is an open-source command-line tool and framework
for data transformation workflows. There is also a dbt cloud for managed, hosted
 dbt services with extended capabilities. dbt (core) utilizes .sql, .py, .yml,
 and .bash or any cli for orchestrating data warehouse command executions and
generating .json artifacts.

dbt is agnostic to cloud provider, warehouse, and sql dialect. dbt works most
effectively with version controlled systems and is implemented commonly with
[git](git.html.markdown). dbt leverages [jinja](jinja.html.markdown) functions
for dynamic values within [yaml](yaml.html.markdown), [sql](sql.html.markdown)
and [python](python.html.markdown) .

### dbt SQL Models

```sql
-- models can have optional configs specified within them
{{ config( 
--  names in database will be filenames unless an alias
    alias='report' ,
--  models can be views, tables, incremental (updated tables), 
-- ephemeral (temp tables), and snapshots (historical tables)
    materialized='incremental',
--  if incremental, data capture methods can be specified with a strategy 
    incremental_strategy='delete+insert',
    -- that can fail if the data schema changes
    on_schema_change='fail'
-- 
)}}

-- dbt emphasizes cte based development
with staging_data as (
    select * ,
        current_timestamp() as loaded_date
-- with refs to use other dbt seeds and models
    from {{ ref('seed_data') }}
)

select
    _id,
    account_id,
    transaction_date,
    total_usd,
    loaded_date

from staging_data
-- dbt can use {% if %} statements for conditional code block executions and
-- is_incremental() to check if the current model is incremental
{% if is_incremental() %}
-- {{ this }} self references the current model
where transaction_date > ( select max(transaction_date) from {{ this }} )
-- the code block is not executed if the if statement is not met
{% endif %}


-- Snapshot models preserve historical data, or slowly changing dimensions
{% snapshot snapshotted_model %}
{{
    config(
-- specifying a unique id 
        unique_key='_id'
-- dbt will create new records for snapshotted data if there are changes in the
-- tracked columns
        strategy='check'
        check_cols=['account_manager'],
        snapshot_date='snapshot_date'
    )
}}

with staging_data as (
    select * from {{ ref('staging_account_transactions')}}
),

final as (
    select
        _id,
        transaction_date,
        account_id,
    -- when a change is detected in account_manager a new row will be saved
        account_manager,
        -- the row can have the more recent date of the snapshot for ordering
        CURRENT_TIMESTAMP() as snapshot_date,
        total_transaction_amount,
    -- saved, reusable sql operations can be performed with dbt macros
        {{ get_account_status('last_transaction_date', 'last_payment_date') 
            }} as account_status,

    from
        account_transactions
)

select * from final
{% endsnapshot %}


-- macros are saved in files like macros/account_management_macros
-- a macro is defined with the variables it's expecting
{% macro get_account_status(last_transaction_date, last_payment_date) %}
-- the sql saved for a macro will be performed on the given fields
case
    when {{ last_transaction_date }} < {{last_payment_date}} and 
        last_payment_date < CURRENT_DATE - INTERVAL '1 year' then 'Dormant'
    when {{ last_transaction_date }} > {{last_payment_date}} - INTERVAL 
        '90 days' then 'Overdue'
    else 'Active'
end
-- % endmacro marks the end of the macro code block
{% endmacro %}

```

## Configurations

```yml
#########################################################
# dbt_project.yml
#########################################################
#
# cli commands are executed in same working directory as dbt_project.yml
# dbt_project.yml will always have following values 
name: organization_project_name        
version: "1.0"                           
profile:  database_profile

# And many optional values with implicit defaults
# like folder locations
model-paths: ["models"]
# or the output of the model 
models:
  organization_project:
    +materialized: view     # Default materialization for models
    # these configurations are hierarchical and will act as defaults for files 
    # without config blocks

#########################################################
# profiles.yml
#########################################################
# The profile specified in dbt_project.yml is defined within `profiles.yml`
database_profile:
# Like all dbt files it can contain hard coded values
  target: hardcoded_target_environment_name
  outputs:
    dev:
        type: postgres
# or environment variables using jinja
        user: "{{ env_var('POSTGRES_USER') }}"
        password: "{{ env_var('POSTGRES_PW') }}"
    # with defaults for variables if not available
        database: "{{ env_var('POSTGRES_DB', 'core') }}"
    # and python augmentation of variables
        schema: "{{ '_'.join([env_var('POSTGRES_USER').replace('.', '_').upper()
        , env_var('POSTGRES_SCHEMA') ]) }}"
        role: "{{ env_var('POSTGRES_ROLE')}}"
```

### CLI Commands

```bash
# cli commands are executed in same working directory as dbt_project.yml

# .csv files are seeded into database
dbt seed
# .sql or .py models are materialized in the database as tables or view
dbt run
# .sql or .yml tests can be performed
dbt test
# models can be materialized, ran, and shapshotted
dbt build
# a command can specify a model
dbt build --select final_model
    # with upstream dependencies
dbt build --select +final_model
    # and / or downstream dependencies
dbt build --select +final_model+
# metadata can be generated on materialized models
dbt docs generate
# full command list available in
dbt list
```

### Repository Structure

```text
dbt has a default file structure when configurations do not define location

repository/
└── dbt/
    ├── dbt_project.yml                        # Required
    ├── profiles.yml                           # Required 
    ├── models/                                # Required , optional name
    │   ├── staging/                           # Optional subfolders
    │   |   └── staging_model.sql
    │   └── final_model.sql
    ├── macros/                                # Optional macro functions
    │   └── custom_macros.sql
    ├── snapshots/                             # Optional snapshot models
    │   └── snapshot.sql
    ├── seeds/                                 # Optional csv files
    │   └── seed_data.csv
    ├── logs/                                  # Output location
    ├── target/                                # Output location
    └── tests/                                 # Optional model tests
        └── custom_tests.sql
```

## Further Reading

* [dbt logging](https://docs.getdbt.com/reference/global-configs/logs) - dbt documentation on outputs logs that can capture execution &
debug logging
* [dbt metadata artifacts](https://docs.getdbt.com/reference/artifacts/dbt-artifacts) - dbt documentation on generated artifacts, such as
json documents for detailing attributes & metadata of a project