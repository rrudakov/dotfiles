#!/bin/sh

######################################################################
# install.sh
# This script creates symlinks in the home firectory for any files in
# ~/dotfiles directory
######################################################################

############### Variables:

dir=~/dotfiles
files="Xresources emacs.d/init.el tmux.conf vimrc xmonad/xmonad.hs xmobarrc xsession gitconfig stalonetrayrc zshrc zshenv aliasrc Xmodmap"

###############

# Changing to the dotfiles directory

echo "Changing to the $dir directory"
cd $dir
echo "...done"

# Move any existing dotfiles to ~/dotfiles_old, then creating symlinks

for file in $files; do
    echo "Creating symlinks for $file in home directory"
    ln -sf $dir/$file ~/.$file
done
