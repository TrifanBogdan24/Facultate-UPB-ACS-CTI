archive_name='322CDa_Trifan_Bogdan_Cristian.zip'

nr_args=$#
script_name=$0

if (( $nr_args != 1 )); then
  echo 'Err: The scripts expect a name of a file'
  echo 'Example: $script_name lab-[0-9*].sc'
  exit 255
fi

file=$1

if [[ ! -f $file ]] ; then
  echo 'Err: The file `$file` was not found'
  exit 255
fi

rm -f $archive_name ~/Downloads/$archive_name

# creating the archive in the current directory
zip $archive_name $file

# copying it in `Downloads`
cp $archive_name ~/Downloads/$archive_name
