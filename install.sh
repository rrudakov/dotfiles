#!/bin/sh

######################################################################
# install.sh
# This script creates symlinks in the home firectory for any files in
# ~/dotfiles directory
######################################################################

############### Variables:

dir=${HOME}/dotfiles
files="Xresources emacs.d/init.el emacs.d/emacs.org emacs.d/custom/google-java-format.el emacs.d/templates/default-java.el tmux.conf vimrc xmonad/xmonad.hs xmobarrc xprofile gitconfig stalonetrayrc zshrc zshenv aliasrc config/dunst/dunstrc config/fontconfig/fonts.conf config/alacritty/alacritty.yml"

###############

# Changing to the dotfiles directory

echo "Changing to the $dir directory"
cd ${dir}
echo "...done"

echo "Create directories"
mkdir -p ${HOME}/.xmonad ${HOME}/.emacs.d/custom ${HOME}/.config/dunst ${HOME}/.config/fontconfig ${HOME}/.config/alacritty ${HOME}/.emacs.d/templates
echo "...done"

# Move any existing dotfiles to ~/dotfiles_old, then creating symlinks

for file in $files; do
    echo "Creating symlinks for $file in home directory"
    ln -sf ${dir}/${file} ${HOME}/.${file}
done
