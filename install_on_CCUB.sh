CCUBlogin='al1966po'

# creating required directories
mkdir ../cgenie_archive
mkdir ../cgenie_output
mkdir ../cgenie_log

# adapting files
sed -i -e "s/CCUBlogin/$CCUBlogin/g" genie-main/user.mak
sed -i -e "s/CCUBlogin/$CCUBlogin/g" genie-main/runmuffin.sh
sed -i -e "s/CCUBlogin/$CCUBlogin/g" genie-main/runmuffin_nocleanall.sh
sed -i -e "s/CCUBlogin/$CCUBlogin/g" genie-main/user.sh


