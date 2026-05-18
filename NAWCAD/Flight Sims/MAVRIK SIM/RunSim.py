"This program is used to run MAVRIK on a Windows machine."

import os
import sys
import subprocess
import time

def main():
    "This is the main function that runs MAVRIK and other programs on a Windows machine."
    # Choose project
    while True:
        # Input Project
        project = input("Enter the name of the simulation you want to run (MAVRIK or PX4QGC): ")
        
        # Check for quit command first
        if project == "q":
            print("\nExiting program. Goodbye!\n")
            break

        # Validate project
        if project not in ("mavrik", "px4qgc"):
            print(f"\nError: '{project}' is not valid. Please enter 'mavrik' or 'px4qgc'.\n")
            continue  # prompt again

        # Call Project Function
        if project == "mavrik":
            build_mavrik.run()
            MAVRIK.run()
            break  # exit after running MAVRIK
        elif project == "px4qgc":
            PX4QGC.run()
            break  # exit after running PX4QGC

class build_mavrik:
    "This class is used to build MAVRIK and other programs on a Windows machine."
    @staticmethod
    def run():
        while True:
            # Input yes or no value
            string = input("Want to build MAVRIK? (yes/no/q): ").strip().lower()

            # Quitting conditional from build
            if string == "q":
                sys.exit("\nExiting program. Goodbye!\n")
                break
            # Conditional to update or not update build.bat
            elif string == "yes":
                mavrik_path = r'C:\Users\brint\Documents\NAWCAD\MAVIStran-dev_mavlink'  # <--- This is the root where build.bat is
                build_bat_path = os.path.join(mavrik_path, 'build.bat')
                try:
                    subprocess.run([build_bat_path], check=True, cwd=mavrik_path)
                    print("\nBuild completed successfully.\n")
                    pass
                except subprocess.CalledProcessError as e:
                    print("\nBuild failed with error:\n", e)
                    continue
                except FileNotFoundError as e:
                    print("\nbuild.bat not found:\n", e)
                    continue
            elif string == "no":
                print("\nBuild cancelled.\n")
                break

class MAVRIK:
    "This class is used to run MAVRIK on a Windows machine."
    @staticmethod # This decorator indicates that the method can be called on the class itself, without needing an object of the class.
    def run():
        # Read the raw string paths for the wireframe and MAVRIK simulation.
        wireframe_path = r"C:\Users\brint\Documents\NAWCAD\Flight Sims\Wireframe" # This is the raw string/root where view.py and input.json are
        mavrik_path = r"C:\Users\brint\Documents\NAWCAD\Flight Sims\MAVRIK SIM" # This is the path to the MAVRIK simulation file. This is the raw string/root where mavrik_sim.exe is.

        # Read the paths to the files needed to run the GUI and simulation.
        view_py_path = os.path.join(wireframe_path, "view.py") # This is the path to view.py, which is the script that runs the MAVRIK GUI. This combines the path to the filename to complete the path to the file.
        input_json_path1 = os.path.join(wireframe_path, "trv150.json") # This is the path to the input file, which is the file that contains the input parameters for the MAVRIK simulation. Same idea as above.
        executable_path = os.path.join(mavrik_path, "mavrik33.exe") # This is the path to mavrik_sim.exe, which is the file that runs the MAVRIK simulation. This combines the path to the filename to complete the path to the file.
        input_json_path2 = os.path.join(mavrik_path, "NAWCAD.json") # This is the path to the input file, which is the file that contains the input parameters for the MAVRIK simulation. Same idea as above.

        # Clear the terminal for better readability
        os.system('cls' if os.name == 'nt' else 'clear') # This command clears the terminal. 'cls' is the command for Windows, and 'clear' is the command for Unix-based systems. os.name is used to determine the operating system.

        # Run MAVRIK GUI
        try:
            GUI = subprocess.Popen(
                [sys.executable, view_py_path, input_json_path1], # Just like in the command prompt, this uses the OS of RunSim.py (Python) to run the view.py script with the input.json file as an argument.
                cwd=wireframe_path # cwd = Current Working Directory. This tells Python to run the command in the wireframe_path directory, which is where view.py and input.json are.
            ) # Popen is used instead of run because we want the GUI to stay open while the simulation runs, and run would wait for the GUI to close before moving on to the next command.
        except subprocess.CalledProcessError as e:
            print("\nMAVRIK GUI failed to run:\n", e)
        except FileNotFoundError as e:
            print("\nCould not find view.py or input.json:\n", e)

        # Delaying the simulation to give the simulation a chance to initialize before the simulation starts.
        time.sleep(5) # Wait 5 seconds

        # Run MAVRIK simulation
        try:
            subprocess.run(
                [executable_path, input_json_path2], # This runs the mavrik_sim.exe file with the input.json file as an argument.
                cwd=mavrik_path, # This tells Python to run the command in the mavrik_path directory, which is where mavrik_sim.exe is.
                check=True
            )
        except subprocess.CalledProcessError as e:
            print("\nMAVRIK simulation failed to run:\n", e)
        except FileNotFoundError as e:
            print("\nCould not find mavrik33.exe or NAWCAD.json:\n", e)
        finally: # Runs no matter what
            print("Closing GUI...\n")
            GUI.terminate() # Closes GUI automatically after simulation is complete

class PX4QGC:
    "This class is used to run PX4 and QGC on a Windows machine."
    @staticmethod
    def run():
        pass
    
if __name__ == "__main__":
    main()