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

end=`date -u -d "7 days" '+%Y-%m-%dT%H:%MZ'`
sastoken=`az storage container generate-sas --account-name ecdcwls --expiry $end --name sendicott --permissions racwdli -o tsv --auth-mode login --as-user`

sasurl=https://ecdcwls.blob.core.windows.net/sendicott/?$sastoken

ghpat=`Rscript -e "cat(gh::gh_token())"`

### Prepare files for each task

# Replace placeholders for secrets in json file
# Using , as delimter for sed to work with url
sed 's,<subnetId>,'${subnetid//&/\\&}',g' cloud/pool_connect.json\
| sed 's,<id>,'${poolName}',g'> cloud/pool_to_use.json

# make a differnt script for each task
mkdir -p cloud/task_jsons

for scei in ML BL MH BH E U I
do 
  sed 's,<SASURL>,'${sasurl//&/\\&}',g' cloud/task_connect.json\
  | sed 's,<sce>,'$scei',g' > cloud/task_jsons/task_connect_$scei.json
done

sed 's,<SASURL>,'${sasurl//&/\\&}',g' cloud/task_connect_combine.json > cloud/task_connect_combine_to_use.json
sed 's,<pat>,'$ghpat',g' make.R > make_to_use.R


#### Move files to container ##############
az storage copy -d $sasurl -s cloud/run_connect.sh
az storage copy -d $sasurl -s make_to_use.R
az storage copy -d $sasurl -s DESCRIPTION
az storage copy -d $sasurl -s data --recursive
az storage copy -d $sasurl -s scripts --recursive
az storage copy -d $sasurl -s outputs --recursive

# to move just one folder to outputs
az storage copy -d https://ecdcwls.blob.core.windows.net/sendicott/outputs/?$sastoken -s outputs/rasters --recursive

#### Create pool, job, tasks ##########################
# AWS machine was m5a.4xlarge with 16 vCPU and 64 gb RAM

az batch pool create --json-file cloud/pool_to_use.json
az batch job create --pool-id $poolName --id $jobName

for scei in ML BL MH BH E U I
do 
  az batch task create --json-file cloud/task_jsons/task_connect_$scei.json --job-id $jobName
done

az batch task create --json-file cloud/task_connect_combine_to_use.json --job-id $jobName

# az batch task reactivate --task-id connectivity-combine --job-id $jobName

# for scei in ML BL MH BH E U I
# do
#   az batch task delete --job-id $jobName --task-id connectivity-rerun-$scei --yes
# done

#### Monitor tasks ############################

# details for a single task filtered by query
az batch task show --job-id $jobName --task-id connectivity-rerun \
--query "{state: state, executionInfo: executionInfo}" --output yaml

# az batch task delete --job-id $jobName --task-id connectivity-rerun --yes
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
az storage copy -s https://ecdcwls.blob.core.windows.net/sendicott/outputs/rasters/?$sastoken \
-d outputs --recursive 

# NOTE removes ***everything*** from the storage container
az storage remove -c sendicott -n outputs/objects --account-name ecdcwls --sas-token $sastoken --recursive

#### Delete pool and job ##########################
az batch job delete --job-id $jobName
az batch pool delete --pool-id $poolName


