#!/bin/bash
#!/bin/bash
printf "\nAlice is (for example) a Plutus expert. Bob needs help with Plutus. Alice is offering 60 minute blocks of time where she offers 1:1 support on Plutus, at a rate of 20 Ada/hour\n"
printf "\n"
export AliceW=`curl -s -d '' http://localhost:9080/wallet/create | jq '.wiWallet.getWalletId'`
sleep 1
export BobOneW=`curl -s -d '' http://localhost:9080/wallet/create | jq '.wiWallet.getWalletId'`
sleep 1
export LoginAuthW=`curl -s -d '' http://localhost:9080/wallet/create | jq '.wiWallet.getWalletId'`
sleep 1
export PlatformW=`curl -s -d '' http://localhost:9080/wallet/create | jq '.wiWallet.getWalletId'`
sleep 1

read -n1 -r -p "Press any key to continue ..." key

export AliceW_IID=$(curl -s -H "Content-Type: application/json" -X POST -d '{"caID": "EscrowContract", "caWallet":{"getWalletId": '$AliceW'}}' http://localhost:9080/api/contract/activate | jq .unContractInstanceId | tr -d '"')
export BobOneW_IID=$(curl -s -H "Content-Type: application/json" -X POST -d '{"caID": "EscrowContract", "caWallet":{"getWalletId": '$BobOneW'}}' http://localhost:9080/api/contract/activate | jq .unContractInstanceId | tr -d '"')
export LoginAuthW_IID=$(curl -s -H "Content-Type: application/json" -X POST -d '{"caID": "AuthNFTIssuerContract", "caWallet":{"getWalletId": '$LoginAuthW'}}' http://localhost:9080/api/contract/activate | jq .unContractInstanceId | tr -d '"')
export PlatformW_IID=$(curl -s -H "Content-Type: application/json" -X POST -d '{"caID": "Login", "caWallet":{"getWalletId": '$PlatformW'}}' http://localhost:9080/api/contract/activate | jq .unContractInstanceId | tr -d '"')
sleep 1




printf "\n"
read -n1 -r -p "Press any key to deposit 20 ADA and see available time of Alice..." key
printf "\n"
curl -H "Content-Type: application/json" -X POST -d 20000000 http://localhost:9080/api/contract/instance/$BobOneW_IID/endpoint/book &&
sleep 2

printf "\n"
read -n1 -r -p "Continue booking or Cancel & redeem deposit?" key
printf "\n"
curl -H "Content-Type: application/json" -X POST -d "false" http://localhost:9080/api/contract/instance/$BobOneW_IID/endpoint/cancel
sleep 2

printf "\n"
read -n1 -r -p "Confirm timing and book session" key
printf "\n"
curl -H "Content-Type: application/json" -X POST -d '{"time":"10-02-2021-4PM-UTC", "cWallet": {"getWalletId": '$BobOneW'}, "pWallet": {"getWalletId": '$LoginAuthW'}}' http://localhost:9080/api/contract/instance/$LoginAuthW_IID/endpoint/book &&
sleep 2

printf "\n"
read -n1 -r -p "Login for the meeting..." key
curl -H "Content-Type: application/json" -X POST -d '{"time":"10-02-2021-4PM-UTC", "clientWallet": {"getWalletId": '$BobOneW'}, "issuerWallet": {"getWalletId": '$LoginAuthW'}}' http://localhost:9080/api/contract/instance/$PlatformW_IID/endpoint/checkAccess
sleep 2

printf "\n"
read -n1 -r -p "CheckerAI checked if the meeting happened and it sends Alice the Amount if the meeting happenned. Press any key to continue..." key
curl -H "Content-Type: application/json" -X POST -d "true" http://localhost:9080/api/contract/instance/$AliceW_IID/endpoint/checkerAI
sleep 2