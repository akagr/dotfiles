if status is-interactive
    # Commands to run in interactive sessions can go here
end

#================================ Path ===============================

# Adds homebrew and installed packages to the path
if test -e /opt/homebrew/bin/brew
    fish_add_path /opt/homebrew/bin
    eval (/opt/homebrew/bin/brew shellenv)
end

if test -e /usr/local/bin/brew
    fish_add_path /usr/local/bin
    eval (/usr/local/bin/brew shellenv)
end

if test -e /opt/homebrew/bin/fzf
    fzf --fish | source
end

if test -e /opt/homebrew/bin/zoxide
    zoxide init fish | source
end

# Adds docker cli if it exists
if test -e $HOME/.docker/bin
  fish_add_path $HOME/.docker/bin
end

# Add GNU sed if it exists
# The default macOS sed doesn't work with standard flags
if test -e /usr/local/opt/gnu-sed/libexec/gnubin
  fish_add_path /usr/local/opt/gnu-sed/libexec/gnubin
end

#========================== General Aliases ==========================
alias du="dust -d 1"
alias t="tmux attach || tmux"
alias vim="nvim"
alias vi="nvim"

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
# Turn off file name completions
complete -c aws-load-credentials -f
# Turn on completion for profile names
complete -c aws-load-credentials -a "(aws configure list-profiles)"
alias alc="aws-load-credentials"

function vterm_printf;
    if begin; [  -n "$TMUX" ]  ; and  string match -q -r "screen|tmux" "$TERM"; end
        # tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$argv"
    else if string match -q -- "screen*" "$TERM"
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$argv"
    else
        printf "\e]%s\e\\" "$argv"
    end
end

function speedtest;
    curl -s "https://raw.githubusercontent.com/sivel/speedtest-cli/master/speedtest.py" | python3 -
end


if test -e $HOME/.colima
    set -x DOCKER_HOST "unix://$HOME/.colima/default/docker.sock"
end

# Suppress CDK warning for new/unsupported node version
set -x JSII_SILENCE_WARNING_UNTESTED_NODE_VERSION true
