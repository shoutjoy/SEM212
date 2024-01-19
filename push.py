import os, sys

def main(argv):
  if len(argv) <= 1:
    commit_name = input("커밋할 이름을 입력하세요: ")
  else:
    commit_name = argv[1]
  os.system("""git add .""")
  os.system(f"""git commit -m "{commit_name}" """)
  os.system("git push origin main")
  
if __name__ == "__main__":
  main(sys.argv)
