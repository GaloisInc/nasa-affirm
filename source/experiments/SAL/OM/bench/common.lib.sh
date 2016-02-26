# common.lib.sh
#
# This script is meant to be sourced. It defines several useful shell script
# utilities.

# print given error message to stderr, does not exit process
#   $1: error message to emit
errmsg ()
{
  echo "ERROR: $1" >&2
}

# "resolve_prog $file" attempts to resolve $file as an executable
# and returns full path to it.
#   $1: executable to resolve
resolve_prog()
{
  local file=$1
  local dir=${file%/*}
  local res=""
  # if $file does not contain a directory, search path.
  if [ "$dir" == "$file" ]; then
    res=$(type -p $file || echo)
    if [ -z $res ]; then
      errmsg "Could not find $file in path."
      return 1
    fi
  elif [ -z $dir ]; then
    # file is in root directory.
    res=$file
  else
    # Build absolute path.
    res="$(cd $dir; pwd)/${file##*/}"
  fi
  if [ ! -f $res ]; then
    errmsg "Could not find $file."
    return 1
  fi
  if [ ! -x $res ]; then
    errmsg "$file is not executable."
    return 1
  fi
  echo "$res"
}

