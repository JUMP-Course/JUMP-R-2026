# JUMP R 语言课程 GitHub Desktop 使用说明

> 适用对象：JUMP 计划 R 语言课程学生
> 使用场景：统一课程 GitHub 仓库；每位学生在自己的文件夹中提交个人项目材料
> 推荐工具：GitHub Desktop，不要求使用命令行 Git

---

## 0. 这份说明要解决什么问题

本课程使用统一的 GitHub 仓库管理每位同学的学习过程和项目材料。每位同学需要完成以下事情：

1. 创建自己的 GitHub 账号；
2. 接受课程仓库的 collaborator 邀请；
3. 安装并登录 GitHub Desktop；
4. 将课程仓库下载到自己的电脑；
5. 在 `students/姓名全拼/` 下创建自己的个人文件夹；
6. 每次课后将代码、图表、报告等文件提交到 GitHub；
7. 最终提交一份完整的 R Markdown 或 Quarto 分析报告。

本课程不要求大家掌握复杂的 Git 命令。只需要学会 GitHub Desktop 中的几个基本操作：

```text
Clone repository → 修改/新增文件 → Commit → Push origin → 在网页端确认
```

---

## 1. 课程仓库使用规则

### 1.1 每位同学只修改自己的文件夹

课程仓库中会设置如下目录结构：

```text
JUMP-R-2026/
├── README.md
├── course_materials/        # 课程资料，学生不要修改
├── templates/               # 模板文件，学生可以复制使用，不要直接修改
├── examples/                # 示例项目，学生可以参考，不要直接修改
└── students/                # 学生个人项目提交区
    ├── zhangsan/
    ├── lisi/
    └── wangwu/
```

每位同学只能在自己的目录下提交材料，例如：

```text
students/zhangsan/
```

不要修改以下内容：

- 不要修改其他同学的文件夹；
- 不要删除或移动课程公共资料；
- 不要直接修改 `course_materials/`、`templates/`、`examples/` 中的文件；
- 不要随意重命名仓库中的公共文件夹。

### 1.2 个人文件夹命名规则

个人文件夹统一使用**姓名全拼小写**，不加空格，不使用中文，例如：

```text
students/zhangsan/
students/lihua/
students/wangxiaoming/
```

如果出现重名，可在后面加数字或班级缩写，例如：

```text
students/zhangsan01/
students/zhangsan02/
```

### 1.3 每次课后都要提交一次

每次课后，请将本次课程对应的代码、图表、数据说明或报告更新到自己的文件夹中。提交不要求完美，但必须能反映真实进展。

---

## 2. 创建 GitHub 账号

### 2.1 打开 GitHub 官网

在浏览器中打开：

```text
https://github.com/
```

点击右上角 **Sign up** 注册账号。

### 2.2 填写注册信息

注册时通常需要填写：

- Email：建议使用自己常用邮箱；
- Password：设置一个安全密码；
- Username：GitHub 用户名，建议使用英文、拼音或英文加数字；
- 邮箱验证码：根据 GitHub 提示完成邮箱验证。

建议用户名尽量简洁，例如：

```text
zhangsan
zhangsan2026
zs_biostat
```

不建议使用过于复杂或难以识别的用户名。

### 2.3 记录自己的 GitHub 用户名

注册完成后，请把自己的 GitHub 用户名填写到课程登记表中。注意：

- GitHub 用户名不是中文姓名；
- GitHub 用户名不是邮箱地址；
- GitHub 用户名通常显示在个人主页链接中，例如：

```text
https://github.com/zhangsan
```

其中 `zhangsan` 就是 GitHub 用户名。

---

## 3. 接受课程仓库 collaborator 邀请

### 3.1 等待发送邀请

课程组会根据大家填写的 GitHub 用户名，统一发送课程仓库 collaborator 邀请。

### 3.2 接受邀请

收到邀请后，通常有两种接受方式。

第一种：通过邮箱接受。

1. 打开注册 GitHub 时使用的邮箱；
2. 查找来自 GitHub 的邀请邮件；
3. 点击邮件中的 **View invitation** 或 **Accept invitation**；
4. 登录 GitHub 后点击接受。

第二种：通过 GitHub 网页通知接受。

1. 登录 GitHub；
2. 查看右上角通知；
3. 找到课程仓库邀请；
4. 点击接受。

### 3.3 接受成功的标志

接受成功后，你应该可以打开课程仓库网页，并看到仓库中的文件列表。如果仓库是私有仓库，未接受邀请前通常无法访问。

### 3.4 如果没有收到邀请怎么办

请按以下顺序检查：

1. 确认自己已经注册 GitHub 账号；
2. 确认课程登记表中的 GitHub 用户名填写正确；
3. 检查邮箱垃圾邮件；
4. 确认自己登录的是被邀请的 GitHub 账号；
5. 如果仍无法接受，请联系老师重新发送邀请。

---

## 4. 安装 GitHub Desktop

### 4.1 下载 GitHub Desktop

在浏览器中打开：

```text
https://desktop.github.com/
```

根据自己的系统下载并安装：

- Windows：下载 Windows 版本；
- macOS：下载 macOS 版本。

### 4.2 登录 GitHub Desktop

安装完成后，打开 GitHub Desktop。

常见登录路径：

```text
File → Options → Accounts → Sign in
```

macOS 上可能是：

```text
GitHub Desktop → Preferences → Accounts → Sign in
```

登录时会跳转到浏览器，请使用自己的 GitHub 账号登录并授权 GitHub Desktop。

### 4.3 设置用户名和邮箱

第一次使用 GitHub Desktop 时，软件可能会提示设置 Git 提交信息：

- Name：可以填写中文姓名或英文名；
- Email：建议使用 GitHub 账号绑定邮箱。

这些信息会显示在提交记录中，便于老师查看是谁提交了文件。

---

## 5. 将课程仓库下载到本地电脑

这个步骤叫做 **Clone repository**，意思是把 GitHub 上的课程仓库复制到自己的电脑上。

### 5.1 方法一：从 GitHub 网页打开 GitHub Desktop

1. 登录 GitHub；
2. 打开课程仓库网页；
3. 点击绿色的 **Code** 按钮；
4. 选择 **Open with GitHub Desktop**；
5. 浏览器会自动打开 GitHub Desktop；
6. 选择本地保存位置；
7. 点击 **Clone**。

建议保存到一个容易找到的位置，例如：

```text
Documents/JUMP-R-2026/
```

不要放在过深、带有中文或特殊符号的路径下。

### 5.2 方法二：直接在 GitHub Desktop 中克隆

1. 打开 GitHub Desktop；
2. 点击：

```text
File → Clone Repository
```

3. 选择 **GitHub.com** 标签页；
4. 找到课程仓库；
5. 选择本地保存路径；
6. 点击 **Clone**。

如果看不到课程仓库，请检查：

- 是否已经接受 collaborator 邀请；
- GitHub Desktop 是否登录了正确账号；
- 是否可以在浏览器中打开课程仓库网页。

---

## 6. 创建自己的个人文件夹

### 6.1 打开本地仓库文件夹

在 GitHub Desktop 中点击：

```text
Repository → Show in Explorer
```

macOS 上可能显示为：

```text
Repository → Show in Finder
```

这会打开你电脑上的课程仓库文件夹。

### 6.2 进入 `students/` 文件夹

在本地课程仓库中找到：

```text
students/
```

然后在里面新建自己的文件夹，例如：

```text
students/zhangsan/
```

> 注意，git默认不会上传空文件夹。所以需要在该文件夹内创建一个文件，比如README.md 文件。才会上传成功。

### 6.3 建议的个人项目结构

每位同学建议按照以下结构整理自己的项目：

```text
students/zhangsan/
├── README.md
├── R/
│   ├── 01_import.R
│   ├── 02_cleaning.R
│   ├── 03_descriptive.R
│   ├── 04_visualization.R
│   └── 05_modeling.R
├── report/
│   └── final_report.qmd
├── figures/
├── tables/
└── output/
```

各文件夹含义如下：

| 文件夹      | 用途                                   |
| ----------- | -------------------------------------- |
| `README.md` | 说明自己的项目题目、数据来源和当前进展 |
| `R/`        | R 脚本                                 |
| `report/`   | R Markdown 或 Quarto 报告              |
| `figures/`  | 导出的图片                             |
| `tables/`   | 导出的表格                             |
| `output/`   | 其他分析结果                           |

---

## 7. 第一次提交：上传自己的文件夹

### 7.1 在本地创建文件

例如，你在 `students/zhangsan/` 中新建了：

```text
README.md
```

并写入以下内容：

```markdown
# 张三的 R 语言课程项目

## 项目题目

待定

## 研究问题

待定

## 数据来源

待定

## 当前进展

- 已创建个人文件夹
- 已完成第一次提交
```

### 7.2 回到 GitHub Desktop

GitHub Desktop 会自动识别你新增或修改的文件。左侧会显示 changed files。

请检查：

- 是否只修改了自己的文件夹；
- 是否没有误删他人文件；
- 是否没有上传无关文件。

### 7.3 填写提交说明

在左下角填写 Summary，例如：

```text
add personal folder for zhangsan
```

Description 可以不填，也可以简单说明：

```text
create README and project folder structure
```

> 建议大家写中文，便于老师查看。

### 7.4 点击 Commit

点击左下角按钮：

```text
Commit to main
```

如果按钮显示的是 `Commit to master`，也可以点击。具体名称取决于仓库默认分支。

### 7.5 点击 Push origin

Commit 只是把修改保存到自己的电脑本地。还需要点击顶部的：

```text
Push origin
```

这样文件才会上传到 GitHub 网页上的课程仓库。

### 7.6 在网页端确认

Push 成功后，打开课程仓库网页，进入：

```text
students/zhangsan/
```

确认自己的文件已经出现在网页上。

---

## 8. 每次课后如何更新文件

每次课后，你都需要重复以下流程：

```text
打开 GitHub Desktop
        ↓
点击 Fetch origin 或 Pull origin
        ↓
打开本地仓库文件夹
        ↓
修改自己文件夹中的代码、图表或报告
        ↓
回到 GitHub Desktop 检查 changed files
        ↓
填写 Summary
        ↓
点击 Commit to main
        ↓
点击 Push origin
        ↓
打开 GitHub 网页确认是否上传成功
```

### 8.1 修改前先 Pull

每次修改前，建议先打开 GitHub Desktop，点击：

```text
Fetch origin
```

如果显示：

```text
Pull origin
```

请先点击 `Pull origin`，把仓库中的最新内容同步到自己的电脑。

这样可以减少冲突。

### 8.2 每次课建议的提交内容

| 课次     | 建议提交内容                 | 示例 commit message         |
| -------- | ---------------------------- | --------------------------- |
| 第 1 次  | 个人文件夹、README、选题卡   | `add topic card`            |
| 第 2 次  | R Project 结构、qmd/Rmd 初稿 | `add project structure`     |
| 第 3 次  | 数据读取脚本、数据来源说明   | `add data import script`    |
| 第 4 次  | 数据字典、变量检查结果       | `add data dictionary`       |
| 第 5 次  | 数据清洗代码、清洗说明       | `add data cleaning script`  |
| 第 6 次  | 描述性统计表                 | `add descriptive table`     |
| 第 7 次  | 数据可视化图表               | `add visualization figures` |
| 第 8 次  | 统计检验结果                 | `add univariate analysis`   |
| 第 9 次  | 基础模型代码和结果           | `add regression model`      |
| 第 10 次 | 报告主体初稿                 | `update report draft`       |
| 第 11 次 | 修改后的完整报告             | `revise full report`        |
| 第 12 次 | 最终报告和个人总结           | `submit final report`       |

### 8.3 提交说明要具体

不建议写：

```text
update
change
homework
```

建议写：

```text
add diabetes data cleaning script
update BMI distribution plot
revise logistic regression result
submit final report
```

> 建议大家写中文，便于老师查看。

---

## 9. 文件上传注意事项

### 9.1 不要上传敏感数据

如果使用课题组数据、临床数据或含有个人信息的数据，不要直接上传原始数据。需要先咨询老师。

不应上传包含以下信息的数据：

- 姓名；
- 身份证号；
- 手机号；
- 住址；
- 住院号；
- 门诊号；
- 可识别个人身份的原始临床数据。

如果数据不方便上传，可以在 `data/README.md` 中说明：

```markdown
# 数据说明

本项目使用的数据涉及隐私，不上传原始数据。
本仓库仅提交代码、变量说明、模拟数据或脱敏后的示例数据。
```

### 9.2 不要上传过大的文件

GitHub 不适合上传特别大的数据文件。一般来说，课程项目应尽量避免上传超过 50 MB 的文件。

如果数据文件较大，可以采用以下方式：

1. 上传一个小型示例数据；
2. 在 README 中写明数据来源和下载方式；
3. 只上传清洗和分析代码；
4. 课上汇报时在本地展示完整结果。

### 9.3 不建议上传这些临时文件

以下文件通常不需要上传：

```text
.Rhistory
.RData
.Rproj.user/
.DS_Store
Thumbs.db
__pycache__/
*.tmp
*.log
```

如果不确定某个文件是否需要上传，可以先问老师。

> 本项目已经创建了.gitignore 文件，其中包含的文件类型会被 Git 自动忽略，不会上传到 GitHub。请不要修改 .gitignore 文件。如果自己的文件没有被git识别且无法上传，可能是这个原因。理论上这里面包含的文件不应该被上传，如果有特殊需要，请联系老师。

---

## 10. 常见问题与解决方法

### 问题 1：我看不到课程仓库

可能原因：

1. 还没有接受 collaborator 邀请；
2. 登录了错误的 GitHub 账号；
3. 老师邀请的用户名填写错误；
4. 邀请已过期。

解决方法：

1. 确认 GitHub 用户名；
2. 检查邮箱和 GitHub 通知；
3. 用浏览器登录 GitHub 后打开课程仓库链接；
4. 如果仍无法访问，联系老师重新邀请。

### 问题 2：GitHub Desktop 中找不到课程仓库

解决方法：

1. 确认已经在网页端接受邀请；
2. 确认 GitHub Desktop 登录的是同一个账号；
3. 尝试从网页端点击 `Code → Open with GitHub Desktop`；
4. 或在 GitHub Desktop 中选择 `File → Clone Repository → URL`，粘贴课程仓库地址。

### 问题 3：我 Commit 了，但网页上看不到文件

原因通常是只 Commit 了，没有 Push。

解决方法：

1. 打开 GitHub Desktop；
2. 查看顶部是否有 `Push origin`；
3. 点击 `Push origin`；
4. 等待上传完成后刷新网页。

### 问题 4：提示需要 Pull origin

说明远程仓库已经有别人提交了新内容，你本地不是最新版。

解决方法：

1. 点击 `Pull origin`；
2. 等同步完成后，再继续修改、Commit 和 Push。

### 问题 5：出现 conflict 冲突

一般是因为两个人修改了同一个文件。课程中最常见原因是误改了公共文件或他人文件。

解决方法：

1. 不要继续乱点；
2. 截图保存错误提示；
3. 联系老师；
4. 说明自己修改了哪些文件。

### 问题 6：误删了别人文件怎么办

不要紧张，但必须立即处理：

1. 不要继续提交新的修改；
2. 立即联系老师；
3. 说明误删了哪些文件；
4. GitHub 通常可以通过历史记录恢复。

### 问题 7：Push 失败怎么办

常见原因：

1. 网络不稳定；
2. 没有权限；
3. 没有先 Pull 最新版本；
4. 登录账号错误。

解决方法：

1. 检查网络；
2. 点击 `Fetch origin` 或 `Pull origin`；
3. 确认 GitHub Desktop 登录账号；
4. 确认已接受仓库邀请；
5. 仍无法解决时联系老师。

---

## 11. 每次提交前的检查清单

提交前请检查：

- [ ] 我只修改了自己的文件夹；
- [ ] 我没有删除或移动公共资料；
- [ ] 我没有修改其他同学的文件；
- [ ] 我的文件名尽量使用英文、数字或下划线；
- [ ] 我的代码、图表或报告放在了正确位置；
- [ ] 我没有上传敏感数据；
- [ ] 我填写了清楚的 commit message；
- [ ] 我已经点击 `Commit`；
- [ ] 我已经点击 `Push origin`；
- [ ] 我已经在 GitHub 网页端确认文件上传成功。

---

## 12. 推荐个人 README 模板

每位同学可以在自己的文件夹下建立 `README.md`，内容如下：

```markdown
# 姓名的 JUMP R 语言课程项目

## 1. 基本信息

- 姓名：
- 姓名全拼：
- 课程主题方向：
- 项目题目：

## 2. 研究问题

请用 1—2 句话说明本项目要回答的问题。

## 3. 数据来源

请说明数据来自哪里，是否为公开数据，是否可以上传。

## 4. 主要变量

- 结局变量：
- 暴露变量/预测变量：
- 协变量：

## 5. 当前进展

- 第 1 次课：
- 第 2 次课：
- 第 3 次课：
- 第 4 次课：
- 第 5 次课：
- 第 6 次课：
- 第 7 次课：
- 第 8 次课：
- 第 9 次课：
- 第 10 次课：
- 第 11 次课：
- 第 12 次课：

## 6. 当前问题

请记录目前遇到的问题。

## 7. 最终提交内容

- [ ] R 脚本
- [ ] 数据字典
- [ ] 描述性统计表
- [ ] 图表
- [ ] 模型结果
- [ ] 最终报告
- [ ] 个人总结
```

---

## 13. 参考链接

- GitHub 官网：https://github.com/
- GitHub Desktop：https://desktop.github.com/
- GitHub 官方文档：创建 GitHub 账号
  https://docs.github.com/en/get-started/start-your-journey/creating-an-account-on-github
- GitHub 官方文档：邀请协作者
  https://docs.github.com/articles/inviting-collaborators-to-a-personal-repository
- GitHub 官方文档：克隆仓库
  https://docs.github.com/articles/cloning-a-repository
- GitHub 官方文档：从 GitHub 克隆仓库到 GitHub Desktop
  https://docs.github.com/en/desktop/adding-and-cloning-repositories/cloning-a-repository-from-github-to-github-desktop
- GitHub 官方文档：GitHub Desktop 入门
  https://docs.github.com/en/desktop/overview/getting-started-with-github-desktop

---

## 14. 一句话总结

每次课后，请记住这个流程：

```text
先 Pull 最新仓库 → 修改自己文件夹 → Commit → Push → 网页确认
```

只要能坚持按照这个流程提交，就可以完成本课程的过程管理要求。
