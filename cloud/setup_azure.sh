#!/bin/bash
#############################################
#####Login etc##################

az login
az batch account login -g EcDc-WLS-rg -n ecdcwlsbatch
az batch pool list


#### Move files to container ##############
end=`date -u -d "20 days" '+%Y-%m-%dT%H:%MZ'`
sastoken=`az storage container generate-sas --account-name ecdcwls --expiry $end --name sendicott --permissions racwdli -o tsv --auth-mode login --as-user`


sasurl=https://ecdcwls.blob.core.windows.net/sendicott/?$sastoken

jobName="sendicott_job_connect"

poolName="sendicott_connectivity"

az storage copy -d $sasurl -s cloud/run_connect.sh
az storage copy -d $sasurl -s ../LSTD-Connectivity-Paper --recursive

# TODO: modify scripts to add subnet id and other secrets from here
# update placeholders in jsons
sed 's/<sastoken>/'${sastoken//&/\\&}'/g' cloud/task_connect.json > cloud/task_to_use.json

#### Create pool, job, tasks ##########################
# AWS machine was m5a.4xlarge with 16 vCPU and 64 gb RAM

az batch pool create --json-file cloud/pool_connect.json
az batch job create --pool-id $poolName --id $jobName

az batch task create --json-file cloud/task_jsons/caribouDemo$i.json --job-id $jobName

#### Monitor tasks ############################

# details for a single task filtered by query
az batch task show --job-id $jobName \
--task-id caribou-demog_sens_batch1 \
--query "{state: state, executionInfo: executionInfo}" --output yaml

# download output file for a task
az batch task file download --task-id caribou-demog_sens_batch1 \
--job-id $jobName --file-path "wd/nohup_1.out" --destination "./nohup_1.out"

# List of all tasks and their state
# See here for making fancy queries https://jmespath.org/tutorial.html
az batch task list --job-id $jobName --query "{tasks: [].[id, state][]}" --output json

# Summary of task counts by state
az batch job task-counts show --job-id $jobName

# Check what results have been added to the storage container
az storage blob list -c sendicott --account-name ecdcwls --sas-token $sastoken \
--query "[].{name:name}" --prefix $setName --output yaml

#### Download results and remove from storage ################################
az storage copy -s https://ecdcwls.blob.core.windows.net/sendicott/$setName/?$sastoken \
-d results --recursive

# NOTE removes ***everything*** from the storage container
az storage remove -c sendicott --account-name ecdcwls --sas-token $sastoken --recursive

#### Delete pool and job ##########################
az batch job delete --job-id $jobName
az batch pool delete --pool-id $poolName


