
## Monorepo Setup

### K8 setup config and secrets

Make sure the following `kubeconfig` files are setup in these locations

- `~/.kube/config_local` for the local development k8 Environment
- `~/.kube/config_test` for the test environment
- `~/.kube/config_production` for the production environment

For convenience you may like to set these following aliases

```fish
alias kl='kubectl --kubeconfig=$HOME/.kube/config_local'
alias kt='kubectl --kubeconfig=$HOME/.kube/config_test'
alias kp='kubectl --kubeconfig=$HOME/.kube/config_production'
```
Make sure that the following namespaces exist for their respective environments

- For local, `pastureen-local`
- For test, `pastureen-test`
- For production, `pastureen-production`

```bash
kl create namespace pastureen-local
kt create namespace pastureen-test
kp create namespace pastureen-production
```
Pull the respective env files then add them to a secret called pastureen-secrets

```
kl create secret generic --from-env-file ./local.env
```

bahblahblah




