#!/usr/bin/python

# Kevin Ellis 2013, 6.868 Final Project

from builder_common import *

import sys
import random

def handle(request):
   data = request.strip().split('|')
   plan = parse_plan(data[0])
   perturbations = map(float, data[1][1:-1].split(','))
   
   reply = []
        
   for perturbation in perturbations:
       world, gnd = make_initial_world()
       boxes = run_plan(world, plan)
       if boxes == None:
           reply.append([])
       else:
           if perturbation < 0.01:
                new_reply = []
                for box in boxes:
                    x   = box.getPos().x
                    z   = box.getPos().z
                    ang = box.getHpr().z
                    new_reply.append((x,z,ang))
                reply.append(new_reply)
           else:
                samples = sample_stability(perturbation, world, boxes)
                reply.append(map(lambda x: (x, 0.0, 0.0), samples))
   
   print str(reply)

random.seed()
handle(sys.argv[1])
