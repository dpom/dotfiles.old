# ~/.bash_profile : executed by the command interpreter for login shells.


export SCRIPTS="$SCRIPTS:.bash_profile"


# include .bashrc if it exists
if [ -f "$HOME/.bashrc" ]; then
  . "$HOME/.bashrc"
fi

# include .bashrc if it exists
if [ -f "$HOME/.local/.bashrc" ]; then
  . "$HOME/.local/.bashrc"
fi




