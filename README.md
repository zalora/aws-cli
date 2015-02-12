cl-aws
=======

AWS CLI based on [brendanhay/amazonka](https://github.com/brendanhay/amazonka)

Currently supported commands:

```
create-alarm --alarmName <alarmName>
             --metricName <metricName>
             --region <region>
             --namespace <namespace>
             --statistic <statistic>
             --period <period>
             --evaluationPeriods <evaluationPeriods>
             --threshold <threshold>
             --comparisonOperator <comparisonOperator>
             --topicArn <topicArn>
             [--dimension <dimension>]
             [--unit <unit>]
```
```
put-metric --region <region>
           --namespace <namespace>
           --metricName <metricName>
           --value <value>
           [--dimension <dimension>]
           [--unit <unit>]
```
