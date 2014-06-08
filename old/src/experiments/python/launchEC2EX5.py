#!/usr/bin/env python
# EX5: launch EC2 instances to run EX5 (sym reg with varying tasks et difficulties)
####################

import sys
from glob import glob
import subprocess 
import boto
import boto.manage.cmdshell as cmdshell
from boto.exception import EC2ResponseError
import time

EC2_KEYPAIR_NAME = 'ec2-sample-key'
IMAGE_ID = 'ami-e57fea8c' 

def launch_spot_instances(count=1):
    ec2 = boto.connect_ec2()
    spot_instance_info = {
        'price' : .04,
        'key_name' : EC2_KEYPAIR_NAME,
        'image_id' : IMAGE_ID,
        'count' : count,
        'type' : "one-time",
        'security_groups' : ['quick-start-1'],
        'instance_type' : 'm1.large'}
        
    requests = ec2.request_spot_instances(**spot_instance_info)
    return requests


def send_ex5_job(inst, type=None):
    
    script = """
su - ubuntu
cd ~/
rm -fr program-induction
git clone git@bitbucket.org:edechter/program-induction.git >> ~/ec2.ex5.log
cd ~/program-induction/src/PI/
ghc --make -O2 -rtsopts  -main-is Experiments.EX5.main -o runEX5 Experiments/EX5.hs  >> ~/ec2.ex5.log
./runEX5 %i >> ~/ec2.ex5.log
git pull >> ~/ec2.ex5.log
git add data/EX5* >> ~/ec2.ex5.log
git commit -m \'Data from ec2 instance %s running ex5 experiment.\' >> ~/ec2.ex5.log
git push ~/ec2.ex5.log
""" % (type, inst.id)
    



