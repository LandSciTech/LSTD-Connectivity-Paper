#!/bin/bash
#############################################
#####Login etc##################

az login
az batch account login -g EcDc-WLS-rg -n ecdcwlsbatch
az batch pool list

#### Set ids and names ##############
subnetid=$(az network vnet subnet list --resource-group EcDc-WLS-rg --vnet-name EcDc-WLS-vnet \
--query "[?name=='EcDc-WLS_compute-cluster-snet'].id" --output tsv)

jobName="sendicott_job_connect"

poolName="sendicott_connectivity"

#### Move files to container ##############
end=`date -u -d "7 days" '+%Y-%m-%dT%H:%MZ'`
sastoken=`az storage container generate-sas --account-name ecdcwls --expiry $end --name sendicott --permissions racwdli -o tsv --auth-mode login --as-user`

sasurl=https://ecdcwls.blob.core.windows.net/sendicott/?$sastoken

az storage copy -d $sasurl -s cloud/run_connect.sh
az storage copy -d $sasurl -s make.R
az storage copy -d $sasurl -s renv.lock
az storage copy -d $sasurl -s data --recursive
az storage copy -d $sasurl -s scripts --recursive
az storage copy -d $sasurl -s outputs --recursive

#### Create pool, job, tasks ##########################
# AWS machine was m5a.4xlarge with 16 vCPU and 64 gb RAM

# Replace placeholders for secrets in json file
# Using , as delimter for sed to work with url
sed 's,<SASURL>,'${sasurl//&/\\&}',g' cloud/task_connect.json > cloud/task_to_use.json

sed 's,<subnetId>,'${subnetid//&/\\&}',g' cloud/pool_connect.json\
| sed 's,<id>,'${poolName}',g'> cloud/pool_to_use.json

az batch pool create --json-file cloud/pool_to_use.json
az batch job create --pool-id $poolName --id $jobName

az batch task create --json-file cloud/task_to_use.json --job-id $jobName

#### Monitor tasks ############################

# details for a single task filtered by query
az batch task show --job-id $jobName \
--task-id connectivity-rerun \
--query "{state: state, executionInfo: executionInfo}" --output yaml

# az batch task delete --job-id $jobName --task-id connectivity-rerun
# az batch task reactivate --job-id $jobName --task-id connectivity-rerun

# download output file for a task
az batch task file download --task-id connectivity-rerun --job-id $jobName --file-path "stdout.txt" --destination "cloud/stdout.txt"

# List of all tasks and their state
# See here for making fancy queries https://jmespath.org/tutorial.html
az batch task list --job-id $jobName --query "{tasks: [].[id, state][]}" --output json

# Summary of task counts by state
az batch job task-counts show --job-id $jobName

# Check what results have been added to the storage container
az storage blob list -c sendicott --account-name ecdcwls --sas-token $sastoken \
--query "[].{name:name}" --output yaml

#### Download results and remove from storage ################################
az storage copy -s https://ecdcwls.blob.core.windows.net/sendicott/$setName/?$sastoken \
-d results --recursive

# NOTE removes ***everything*** from the storage container
az storage remove -c sendicott --account-name ecdcwls --sas-token $sastoken --recursive

#### Delete pool and job ##########################
az batch job delete --job-id $jobName
az batch pool delete --pool-id $poolName


