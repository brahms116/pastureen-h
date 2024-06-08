# Pastureen

This is a web project for running automations and potentially a blog in the future.

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

Create the secrets `pt-secrets` in each environment

```
kl create secret generic pt-secrets 
kt create secret generic pt-secrets
kp create secret generic pt-secrets
```

Edit each secret to contain the following credentials

- `PT_TODOIST_TOKEN` -> The access token required for the Todoist API
- `PT_NTFY_TOPIC` -> The ntfy sh topic which is used to push notifications

### Deloying Containers and Images

*** Still need to streamline this and figure out stuff ***

Read `./docker/README.MD` for information about building and pushing the conatiner images so that
the terraform project can reference it


### Terraform variable setup

Create the following `TF_VAR`s in your shell environment

- `TF_VAR_application_root` -> The absolute path to the haskell stack application root
- `TF_VAR_stack_dir` -> The absolute path to the global stack directory
- `TF_VAR_data_dir` -> The absolute path to the postgres db data dir for local development
- `TF_VAR_db_test_data_dir` -> The absolute path to the postgres db data dir for test development
- `TF_VAR_noco_meta_dir` -> The absolute path for the nocodb volume, dunno why this is needed
- `TF_VAR_noco_test_meta_dir` -> The absolute path for the test nocodb volume, dunno why this is needed


```fish
set -gx TF_VAR_application_root "/Users/david/dev/pastureen-h/application"
set -gx TF_VAR_stack_dir "/Users/david/.stack-k8"
set -gx TF_VAR_db_data_dir "/Users/david/pg-data"
set -gx TF_VAR_db_test_data_dir "/Users/david/pg-test-data"
set -gx TF_VAR_noco_meta_dir "/Users/david/noco"
set -gx TF_VAR_noco_test_meta_dir "/Users/david/noco-test"
```

### Running terraform and db migrations

Execute the terraform and db migrations via the util scripts directory.

***Make sure the correct environment is selected in util-scripts/Main.hs` This still needs some work ***

```fish
cd util-scripts && stack run
```
