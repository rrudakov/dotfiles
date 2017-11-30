"-------------------------
"" Базовые настройки
"-------------------------
"
" Включаем несовместимость настроек с Vi (ибо Vi нам и не понадобится).
set nocompatible

" Показывать положение курсора всё время.
set ruler		

" Показывать незавершённые команды в статусбаре
set showcmd		

" Включаем нумерацию строк
set nu

" Фолдинг по отсупам
" set foldmethod=indent

" Поиск по набору текста (очень полезная функция)
set incsearch

" Отключаем подстветку найденных вариантов, и так всё видно.
set nohlsearch

" Теперь нет необходимости передвигать курсор к краю экрана, чтобы подняться
" в режиме редактирования
set scrolljump=7

" Теперь нет необходимости передвигать курсор к краю экрана, чтобы
" опуститься в режиме редактирования
set scrolloff=7

" Выключаем надоедливый звонок
set novisualbell
set t_vb= 

" Поддержка мыши
set mouse-=a "запретить визуальный режим при выделении мышью
" set mouse+=a
set mousemodel=popup

" Кодировка текста по умолчанию
set termencoding=utf-8

" Не выгружать буфер, когда переключаемся на другой
" Это позволяет редактировать несколько файлов в один и тот же момент без
" необходимости сохранения каждый раз
" когда переключаешься между ними
set hidden

" Скрыть панель в gui версии ибо она не нужна
set guioptions-=T

" Сделать строку команд высотой в одну строку
set ch=1

" Скрывать указатель мыши, когда печатаем
set mousehide

" Включить автоотступы
" set autoindent

" Преобразование Таба в пробелы
set expandtab

" Размер табулации по умолчанию
"set shiftwidth=4
"set softtabstop=4
"set tabstop=4

" Формат строки состояния
set statusline=%<%f%h%m%r\ %b\ %{&encoding}\ 0x\ \ %l,%c%V\ %P 
set laststatus=2

" Включаем умные отспупы ( например, автоотступ после {)
"set smartindent

" Fix <Enter> for comment

" set fo+=cr

" Опции сесссий
set sessionoptions=curdir,buffers,tabpages

set backupdir=~/.vimbak/

nmap yy yy:silent .w !xclip<cr>
vmap y y:silent '<,'> w !xclip<cr>

set t_Co=256              
set modeline

let base16colorspace=256
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'chriskempson/base16-vim'
Plugin 'morhetz/gruvbox'

call vundle#end()            " required
filetype plugin indent on    " required

syntax enable
set termguicolors
" set background=dark    " Setting dark mode
" let g:gruvbox_italic=1
" let g:gruvbox_contrast_dark='hard'
" let g:gruvbox_invert_selection=0
" colorscheme gruvbox
colorscheme base16-tomorrow-night
