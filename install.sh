#!/usr/bin/env bash
python3 -m venv venvcompiler
source venvcompiler/bin/activate
sudo apt-get install -y graphviz graphviz-dev
sudo apt-get install gcc-multilib
pip install -r requirements.txt
