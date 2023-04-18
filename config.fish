if status is-interactive
    # Commands to run in interactive sessions can go here
end

#================================ Path ===============================

# Adds homebrew and installed packages to the path
if test -e /opt/homebrew/bin/brew
    eval (/opt/homebrew/bin/brew shellenv)
end

# Adds completions for asdf version manager to fish
if test -e /opt/homebrew/opt/asdf/libexec/asdf.fish
    source /opt/homebrew/opt/asdf/libexec/asdf.fish
end

#========================== General Aliases ==========================
alias du="dust -d 1"

#=============================== DevOps ==============================
alias k="kubectl"
alias tf="terraform"

# Adds completions for aws to fish
complete --command aws --no-files --arguments '(begin; set --local --export COMP_SHELL fish; set --local --export COMP_LINE (commandline); aws_completer | sed \'s/ $//\'; end)'

# Restart gpg agent
function restart-gpg-agent -d "restart gpg agent to fix common git remote issues when using https"
  gpgconf --kill gpg-agent
end

# AWS uses identity center to manage SSO access using CLI
# If SSO is not configured, run `aws configure sso` first.
# Then, whenever a session expires or a new one is needed, run `aws sso login`.
# That will generate and store temporary AWS credentials in cache.
# `aws_load_credentials` is a helper to then load these credentials to standard
# AWS environment variables like AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY etc.
function aws-load-credentials -d "load aws credentials from sso cache file to environment variables"
    if test (count $argv) != 1
        echo "Example: aws_load_credentials <aws_profile_name>" >&2
        return 1
    end
    eval $(aws configure export-credentials --profile $argv --format env)
end