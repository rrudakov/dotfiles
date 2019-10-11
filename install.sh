#!/bin/sh

######################################################################
# install.sh
# This script creates symlinks in the home firectory for any files in
# ~/dotfiles directory
######################################################################

############### Variables:

dir=${HOME}/dotfiles
files="offlineimaprc \
  Xresources \
  emacs.d/early-init.el \
  emacs.d/init.el \
  emacs.d/emacs.org \
  emacs.d/custom/google-java-format.el \
  emacs.d/templates/default-java.el \
  tmux.conf \
  vimrc \
  xmonad/xmonad.hs \
  xmobarrc \
  xprofile \
  gitconfig \
  stalonetrayrc \
  bashrc \
  bash_profile \
  aliasrc \
  config/dunst/dunstrc \
  config/fontconfig/fonts.conf \
  config/alacritty/alacritty.yml"

###############

# Changing to the dotfiles directory

echo "Changing to the $dir directory"
cd ${dir}
echo "...done"

echo "Create directories"
mkdir -p ~/.local/bin
mkdir -p ${HOME}/.xmonad ${HOME}/.emacs.d/custom ${HOME}/.config/dunst ${HOME}/.config/fontconfig ${HOME}/.config/alacritty ${HOME}/.emacs.d/templates
echo "...done"

# Move any existing dotfiles to ~/dotfiles_old, then creating symlinks
for file in $files; do
    echo "Creating symlinks for $file in home directory"
    ln -sf ${dir}/${file} ${HOME}/.${file}
done

# Install switch monitor script
ln -sf ${dir}/switch_monitor.sh ${HOME}/.local/bin/switch_monitor
ln -sf ${dir}/update_mail_index.sh ${HOME}/.local/bin/update_mail_index
