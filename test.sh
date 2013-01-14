csi -s tests/run.scm

if [ $? -ne 0 ]; then
  echo -e "\e[0;31mERRORS FOUND\e[00m"
else
  echo -e "\e[0;32mTESTS OK\e[00m"
fi
