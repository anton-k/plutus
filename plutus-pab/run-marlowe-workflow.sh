#!/bin/bash

set -ex

# ----------------------------------------------------------------
# 1. create a wallet (remember its ID and pubKeyHash):
output=$(curl -s -d '' http://localhost:9080/wallet/create)
walletId=$(echo "$output" | sed -r -n 's/.*:([0123456789]*)\}.*/\1/p')
pubKeyHash=$(echo "$output" | jq -r '.wiPubKeyHash.getPubKeyHash')


# ----------------------------------------------------------------
# 2. activate a wallet companion contract for this wallet (remember its instance ID):
companionAppPath=$(cabal exec -- which marlowe-companion-app)
json=$(cat <<-END
  { "caID":
      { "contractPath":"${companionAppPath}"},
       "caWallet":{"getWallet":${walletId}}
  }
END
)
companionInstanceId=$(curl -s -H "Content-Type: application/json" \
  -d "$json" http://localhost:9080/api/new/contract/activate \
  | jq -r '.unContractInstanceId')


# ----------------------------------------------------------------
# 3. activate a marlowe plutus contract (remember its instance ID)
marloweAppPath=$(cabal exec -- which marlowe-app)
json=$(cat <<-END
  { "caID":
      { "contractPath":"${marloweAppPath}"},
       "caWallet":{"getWallet":${walletId}}
  }
END
)
appInstanceId=$(curl -s -H "Content-Type: application/json" \
  -d "$json" http://localhost:9080/api/new/contract/activate \
  | jq -r '.unContractInstanceId')

sleep 5

# ----------------------------------------------------------------
# 4. use the marlowe plutus contract you just activated to create a marlowe
# contract, and give all the roles to the wallet from step 1
json=$(cat <<-END
  [
    [
      [ { "unTokenName": "Investor" }, { "getPubKeyHash": "${pubKeyHash}" } ],
      [ { "unTokenName": "Issuer" }, { "getPubKeyHash": "${pubKeyHash}" } ]
    ],
    {
      "when": [
        {
          "then": {
            "token": { "token_name": "", "currency_symbol": "" },
            "to": { "party": { "role_token": "Issuer" } },
            "then": {
              "when": [
                {
                  "then": {
                    "token": { "token_name": "", "currency_symbol": "" },
                    "to": { "party": { "role_token": "Investor" } },
                    "then": "close",
                    "pay": 20,
                    "from_account": { "role_token": "Issuer" }
                  },
                  "case": {
                    "party": { "role_token": "Issuer" },
                    "of_token": { "token_name": "", "currency_symbol": "" },
                    "into_account": { "role_token": "Issuer" },
                    "deposits": 20
                  }
                }
              ],
              "timeout_continuation": "close",
              "timeout": 26936589
            },
            "pay": 10,
            "from_account": { "role_token": "Investor" }
          },
          "case": {
            "party": { "role_token": "Investor" },
            "of_token": { "token_name": "", "currency_symbol": "" },
            "into_account": { "role_token": "Investor" },
            "deposits": 10
          }
        }
      ],
      "timeout_continuation": "close",
      "timeout": 26936589
    }
  ]
END
)

curl -s -H "Content-Type: application/json" \
  -d "$json" \
  "http://localhost:9080/api/new/contract/instance/${appInstanceId}/endpoint/create" \

while true; do
  states=$(curl -s \
    "http://localhost:9080/api/new/contract/instance/${companionInstanceId}/status" \
    \ | jq '.cicCurrentState.observableState | length')

  echo "found ${states} item(s)"
  sleep 1

done
