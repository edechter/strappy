#!/usr/bin/env python

import sys
import subprocess 
import boto
import time

def launch_instance(sizes):
    sizes_str = " ".join([str(a) for a in sizes])
    EC2_KEYPAIR_NAME = 'ec2-sample-key'
    IMAGE_ID = 'ami-95aa20fc' 
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

def launch_symreg(inst, sizes):

    sizes_str = " ".join([str(a) for a in sizes])
    script = """
cd ~/
git clone git@bitbucket.org:edechter/program-induction.git
cd ~/program-induction/src/PI/
ghc --make -O2 -rtsopts  -main-is Experiments.SymbolicRegression.main -o runSymReg Experiments/SymbolicRegression.hs 
./runSymReg %s
git pull
git add data/R*
git commit -m \'Data from ec2 instance %s running symreg on sizes %s\'
git push
""" % (sizes_str, inst.id, sizes_str)

    dns = inst.public_dns_name
    sshcmd = "ssh -o StrictHostKeyChecking=no -i ~/.ssh/ec2-sample-key.pem ubuntu@%s \"%s\"; exit;" % (dns, script)
    print sshcmd
    print "Waiting a few minutes to make sure ssh server is ready..."
    time.sleep(60*3)
    print "Okay, ready to continue."
    out = subprocess.Popen(sshcmd, shell=True, stderr=None, stdout=None)
    return out 

def launch_instance_and_symreg(sizes):
    inst = launch_instance(sizes)
    out = launch_symreg(inst,sizes)
    return out

if __name__ == "__main__":

    sizes = [1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000]
    for i in range(10):
        launch_instance_and_symreg(sizes)
 #     sizes = [int(i) for i in sys.argv[1:]]
#     launch_instance_and_symreg(sizes)
    



