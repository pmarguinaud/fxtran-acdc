host=$(hostname)

if [ $host = "mi300x" ]
then
  source ./env.muncl01.sh
fi

if [[ $host =~ (taranis|belenos) ]] 
then 
  source ./env.meteo.sh
fi


