
banned_eapi_4 = files('banned_in_eapi_4')
all_banned_4 = []
foreach bannedscript : ['dohard', 'dosed']
  banned = fs.copyfile(banned_eapi_4, bannedscript)
  all_banned_4 += banned
endforeach
