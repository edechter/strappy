#!/usr/bin/env python

import sys
from glob import glob
import subprocess 
import boto
import time

EC2_KEYPAIR_NAME = 'ec2-sample-key'
IMAGE_ID = 'ami-95aa20fc' 

def launch_instance():
    ec2 = boto.connect_ec2()
    reservation = ec2.run_instances(
        image_id = IMAGE_ID,
        key_name = EC2_KEYPAIR_NAME,
        instance_type="m1.large",
        security_groups = ["quick-start-1"]
    )
    inst = reservation.instances[0]
    
    print "Pending."
    while not inst.state == u'running':
        inst.update()
        pass
    print "Running."
    
    while inst.public_dns_name == '':
        inst = ec2.get_all_instances(instance_ids=[inst.id])[0].instances[0]
           
    return inst

def launch_digarith(inst):

    script = """
cd ~/
git clone git@bitbucket.org:edechter/program-induction.git
cd ~/program-induction/src/PI/
ghc --make -O2 -rtsopts  -main-is Experiments.DigitalArithmetic.main -o runDigArith Experiments/DigitalArithmetic.hs 
./runDigArith
git pull
git add data/R*
git commit -m \'Data from ec2 instance %s running digarith experiment\'
git push
""" % (inst.id)

    dns = inst.public_dns_name
    sshcmd = "ssh -o StrictHostKeyChecking=no -i ~/.ssh/ec2-sample-key.pem ubuntu@%s \"%s\"; exit;" % (dns, script)
    print sshcmd
    print "Waiting a few minutes to make sure ssh server is ready..."
    time.sleep(60*1)
    print "Okay, ready to continue."
    logfilename = "./ec2.digarith.%s.log" % inst.id
    fid = open(logfilename, 'w')
    out = subprocess.Popen(sshcmd, shell=True, stderr=fid, stdout=fid)
    return out 

def launch_instance_and_digarith():
    inst = launch_instance()
    out = launch_digarith(inst)
    return out

def push_all():
    ec2 = boto.connect_ec2()
    reservations = ec2.get_all_instances()
    instances = [r.instances[0] for r in reservations]
    def sendssh(inst):
        dns = inst.public_dns_name
        script = """
cd ~/program-induction/src/PI
git pull
git add data/*
git commit -m \'Data from ec2 instance %s running digarith experiment.\'
git push
""" % inst.id
        sshcmd = "ssh -o StrictHostKeyChecking=no "\
            + "-i ~/.ssh/ec2-sample-key.pem ubuntu@%s \"%s\"; exit;" % (dns, script)
        out = subprocess.Popen(sshcmd, shell=True, stderr=subprocess.STDOUT)
        return out
    for inst in instances:
        print "Connecting to instance %s" % inst.id
        sendssh(inst)
        time.sleep(15)
        print "Finished."

files = glob("../data/digarith_ec2_allbool2and3_overnight_wjittermany*/numhit.csv")
def loadFile(file):
    x = open(file, 'r').read()
    return [float(i) for i in x.split(",")]

    

        
    
    

if __name__ == "__main__":
    for i in range(10): 
        launch_instance_and_digarith()

    



