package mill.resolve

import mainargs.{MainData, TokenGrouping}
import mill.define.internal.Reflect
import mill.define.{
  BaseModule,
  Command,
  Discover,
  Module,
  NamedTask,
  Segment,
  Segments,
  Segment,
  TaskModule,
  SelectMode
}
import mill.api.Result
import mill.resolve.ResolveCore.{Resolved, makeResultException}

private[mill] object Resolve {
  object Segments extends Resolve[Segments] {
    private[mill] def handleResolved(
        rootModule: BaseModule,
        resolved: Seq[Resolved],
        args: Seq[String],
        selector: Segments,
        nullCommandDefaults: Boolean,
        allowPositionalCommandArgs: Boolean,
        resolveToModuleTasks: Boolean,
        cache: ResolveCore.Cache
    ) = {
      // For qualified super tasks (super.Something), return the original selector
      // Otherwise, return the resolved segments
      val hasQualifiedSuper = selector.render.contains(".super.")
      Result.Success(if (hasQualifiedSuper) Seq(selector) else resolved.map(_.segments))
    }

    private[mill] override def deduplicate(items: List[Segments]): List[Segments] = items.distinct
  }

  /**
   * HACK: Dummy task used to wrap [[Module]]s so they can participate in
   * `resolve`/`inspect`/etc.
   */
  class ModuleTask[+T](val module: Module) extends NamedTask[T] {
    override def ctx0 = module.moduleCtx
    override def isPrivate = None
    override val inputs = Nil
    override def evaluate0 = ???
  }

  object Tasks extends Resolve[NamedTask[Any]] {
    private[mill] def handleResolved(
        rootModule: BaseModule,
        resolved: Seq[Resolved],
        args: Seq[String],
        selector: Segments,
        nullCommandDefaults: Boolean,
        allowPositionalCommandArgs: Boolean,
        resolveToModuleTasks: Boolean,
        cache: ResolveCore.Cache
    ) = {

      val taskList: Seq[Result[Option[NamedTask[?]]]] = resolved.map {
        case r: Resolved.NamedTask =>
          val superIdx = r.segments.value.indexWhere {
            case Segment.Label("super") => true
            case _ => false
          }
          
          if (superIdx >= 0) {
            // For tasks with super, create a task with segments before the super segment
            val segmentsWithoutSuper = mill.define.Segments(r.segments.value.take(superIdx))
            Result.Success(Some(new NamedTask[Any] {
              override def ctx0 = mill.define.Ctx.makeRoot(
                millModuleEnclosing0 = rootModule.moduleCtx.enclosing,
                millModuleLine0 = rootModule.moduleCtx.lineNum,
                millSourcePath = rootModule.moduleCtx.millSourcePath,
                segments0 = segmentsWithoutSuper,
                external0 = rootModule.moduleCtx.external,
                fileName = rootModule.moduleCtx.fileName
              )
              override def isPrivate = None
              override val inputs = Nil
              override def evaluate0 = ???
            }))
          } else {
            val instantiated = ResolveCore
              .instantiateModule(rootModule, r.segments.init, cache)
              .flatMap(instantiateNamedTask(r, _, cache))
            instantiated.map(Some(_))
          }

        case r: Resolved.SuperTask =>
          val instantiated = ResolveCore
            .instantiateModule(rootModule, r.segments.init, cache)
            .flatMap(mod =>
              instantiateSuperTask(r, mod, cache)
            )
          instantiated.map(Some(_))

        case r: Resolved.Command =>
          val instantiated = ResolveCore
            .instantiateModule(rootModule, r.segments.init, cache)
            .flatMap { mod =>
              instantiateCommand(
                rootModule,
                r,
                mod,
                args,
                nullCommandDefaults,
                allowPositionalCommandArgs
              )
            }
          instantiated.map(Some(_))

        case r: Resolved.Module =>
          ResolveCore.instantiateModule(rootModule, r.segments, cache).flatMap {
            case value if resolveToModuleTasks => Result.Success(Some(new ModuleTask(value)))
            case value: TaskModule if !resolveToModuleTasks =>
              val directChildrenOrErr = ResolveCore.resolveDirectChildren(
                rootModule,
                value.getClass,
                Some(value.defaultCommandName()),
                value.moduleSegments,
                cache = cache
              )

              directChildrenOrErr.flatMap(directChildren =>
                directChildren.head match {
                  case r: Resolved.NamedTask => instantiateNamedTask(r, value, cache).map(Some(_))
                  case r: Resolved.SuperTask =>
                    instantiateSuperTask(r, value, cache).map(Some(_))
                  case r: Resolved.Command =>
                    instantiateCommand(
                      rootModule,
                      r,
                      value,
                      args,
                      nullCommandDefaults,
                      allowPositionalCommandArgs
                    ).map(Some(_))
                  case _ => ???
                }
              )
            case _ => Result.Success(None)
          }
      }

      val sequenced = Result.sequence(taskList).map(_.flatten)

      sequenced.flatMap(flattened =>
        if (flattened.nonEmpty) Result.Success(flattened)
        else Result.Failure(s"Cannot find default task to evaluate for module ${selector.render}")
      )
    }

    private[mill] override def deduplicate(items: List[NamedTask[Any]]): List[NamedTask[Any]] =
      items.distinctBy(_.ctx.segments)
  }

  private def instantiateNamedTask(
      r: Resolved.NamedTask,
      p: Module,
      cache: ResolveCore.Cache
  ): Result[NamedTask[?]] = {
    val definition = Reflect
      .reflect(
        p.getClass,
        classOf[NamedTask[?]],
        _ == r.segments.last.value,
        true,
        getMethods = cache.getMethods
      )
      .head

    ResolveCore.catchWrapException(
      definition.invoke(p).asInstanceOf[NamedTask[?]]
    )
  }

  private def instantiateSuperTask(
      r: Resolved.SuperTask,
      p: Module,
      cache: ResolveCore.Cache
  ): Result[NamedTask[?]] = {
    // Get the qualifier (if any) from the SuperTask
    val qualifier = r.qualifier

    // The base task name is what comes before ".super" in the segment
    // For "foo.super", the base task name is "foo"
    // For a qualified super task, we need to extract from the original segment
    val baseTaskName = {
      // Try to find the segment that has ".super" in it
      val segmentWithSuper = r.segments.value.find {
        case Segment.Label(name) => name.contains(".super")
        case _ => false
      }

      segmentWithSuper match {
        case Some(Segment.Label(name)) =>
          // Extract the part before ".super"
          name.split("\\.super").head
        case _ =>
          // Fallback: try using the last non-qualifier segment
          val relevantSegment = if (qualifier.isDefined && r.segments.value.length > 1) {
            // If there's a qualifier, use the second-to-last segment
            r.segments.value(r.segments.value.length - 2)
          } else {
            // Otherwise use the last segment
            r.segments.last
          }

          relevantSegment match {
            case Segment.Label(name) =>
              if (name.contains(".super")) name.split("\\.super").head
              else name
            case _ =>
              throw new IllegalArgumentException(
                s"Cannot extract base task name from segments: ${r.segments.render}"
              )
          }
      }
    }

    // Find and instantiate the base task
    val definition = Reflect
      .reflect(
        p.getClass,
        classOf[NamedTask[?]],
        _ == baseTaskName,
        true,
        getMethods = cache.getMethods
      )
      .head

    ResolveCore.catchWrapException {
      val baseTask = definition.invoke(p).asInstanceOf[NamedTask[?]]

      new NamedTask[Any] {
        override def ctx0 = baseTask.ctx0.withSegments(r.segments)
        override def isPrivate = baseTask.isPrivate
        override val inputs = baseTask.inputs
        override def evaluate0 = baseTask.evaluate0

        override def toString = baseTask.toString
      }
    }
  }

  private def instantiateCommand(
      rootModule: BaseModule,
      r: Resolved.Command,
      p: Module,
      args: Seq[String],
      nullCommandDefaults: Boolean,
      allowPositionalCommandArgs: Boolean
  ) = {
    ResolveCore.catchWrapException {
      val invoked = invokeCommand0(
        p,
        r.segments.last.value,
        rootModule.moduleCtx.discover,
        args,
        nullCommandDefaults,
        allowPositionalCommandArgs
      )

      invoked.head
    }.flatMap(x => x)
  }

  private def invokeCommand0(
      target: mill.define.Module,
      name: String,
      discover: Discover,
      rest: Seq[String],
      nullCommandDefaults: Boolean,
      allowPositionalCommandArgs: Boolean
  ): Option[Result[Command[?]]] = for {
    ep <- discover.resolveEntrypoint(target.getClass, name)
  } yield {
    def withNullDefault(a: mainargs.ArgSig): mainargs.ArgSig = {
      if (a.default.nonEmpty) a
      else if (nullCommandDefaults) {
        a.copy(default =
          if (a.reader.isInstanceOf[SimpleTaskTokenReader[?]])
            Some(_ => mill.define.Task.Anon(null))
          else Some(_ => null)
        )
      } else a
    }

    val flattenedArgSigsWithDefaults = ep
      .flattenedArgSigs
      .map { case (arg, term) => (withNullDefault(arg), term) }

    mainargs.TokenGrouping.groupArgs(
      rest,
      flattenedArgSigsWithDefaults,
      allowPositional = allowPositionalCommandArgs,
      allowRepeats = false,
      allowLeftover = ep.argSigs0.exists(_.reader.isLeftover),
      nameMapper = mainargs.Util.kebabCaseNameMapper
    ).flatMap { (grouped: TokenGrouping[Any]) =>
      val mainData = ep.asInstanceOf[MainData[Any, Any]]
      val mainDataWithDefaults = mainData
        .copy(argSigs0 = mainData.argSigs0.map(withNullDefault))

      mainargs.Invoker.invoke(
        target,
        mainDataWithDefaults,
        grouped
      )
    } match {
      case mainargs.Result.Success(v: Command[_]) => Result.Success(v)
      case mainargs.Result.Failure.Exception(e) =>
        Result.Failure(makeResultException(e, new Exception()).left.get)
      case f: mainargs.Result.Failure =>
        Result.Failure(
          mainargs.Renderer.renderResult(
            ep,
            f,
            totalWidth = 100,
            printHelpOnError = true,
            docsOnNewLine = false,
            customName = None,
            customDoc = None,
            sorted = true,
            nameMapper = mainargs.Util.nullNameMapper
          )
        )
      case _ => ???
    }
  }
}

private[mill] trait Resolve[T] {
  private[mill] def handleResolved(
      rootModule: BaseModule,
      resolved: Seq[Resolved],
      args: Seq[String],
      segments: Segments,
      nullCommandDefaults: Boolean,
      allowPositionalCommandArgs: Boolean,
      resolveToModuleTasks: Boolean,
      cache: ResolveCore.Cache
  ): Result[Seq[T]]

  def resolve(
      rootModule: BaseModule,
      scriptArgs: Seq[String],
      selectMode: SelectMode,
      allowPositionalCommandArgs: Boolean = false,
      resolveToModuleTasks: Boolean = false
  ): Result[List[T]] = {
    resolve0(rootModule, scriptArgs, selectMode, allowPositionalCommandArgs, resolveToModuleTasks)
  }

  private[mill] def resolve0(
      rootModule: BaseModule,
      scriptArgs: Seq[String],
      selectMode: SelectMode,
      allowPositionalCommandArgs: Boolean,
      resolveToModuleTasks: Boolean
  ): Result[List[T]] = {
    val nullCommandDefaults = selectMode == SelectMode.Multi
    val resolvedGroups = ParseArgs(scriptArgs, selectMode).flatMap { groups =>
      val resolved = groups.map { case (selectors, args) =>
        val selected = selectors.map { case (scopedSel, sel) =>
          resolveRootModule(rootModule, scopedSel).map { rootModuleSels =>
            resolveNonEmptyAndHandle(
              args,
              rootModuleSels,
              sel.getOrElse(Segments()),
              nullCommandDefaults,
              allowPositionalCommandArgs,
              resolveToModuleTasks
            )
          }
        }

        Result
          .sequence(selected)
          .flatMap(Result.sequence(_))
          .map(_.flatten)
      }

      Result.sequence(resolved)
    }

    resolvedGroups.map(_.flatten.toList).map(deduplicate)
  }

  private[mill] def resolveNonEmptyAndHandle(
      args: Seq[String],
      rootModule: BaseModule,
      sel: Segments,
      nullCommandDefaults: Boolean,
      allowPositionalCommandArgs: Boolean,
      resolveToModuleTasks: Boolean
  ): Result[Seq[T]] = {
    val rootResolved = ResolveCore.Resolved.Module(Segments(), rootModule.getClass)
    val cache = new ResolveCore.Cache()
    val resolved =
      ResolveCore.resolve(
        rootModule = rootModule,
        remainingQuery = sel.value.toList,
        current = rootResolved,
        querySoFar = Segments(),
        seenModules = Set.empty,
        cache = cache
      ) match {
        case ResolveCore.Success(value) => Result.Success(value)
        case ResolveCore.NotFound(segments, found, next, possibleNexts) =>
          val allPossibleNames = rootModule
            .moduleCtx
            .discover
            .classInfo
            .values
            .flatMap(_.declaredTasks)
            .map(_.name)
            .toSet

          Result.Failure(ResolveNotFoundHandler(
            selector = sel,
            segments = segments,
            found = found,
            next = next,
            possibleNexts = possibleNexts,
            allPossibleNames = allPossibleNames
          ))
        case ResolveCore.Error(value) => Result.Failure(value)
      }

    resolved
      .flatMap(r =>
        handleResolved(
          rootModule,
          r.sortBy(_.segments),
          args,
          sel,
          nullCommandDefaults,
          allowPositionalCommandArgs,
          resolveToModuleTasks,
          cache = cache
        )
      )
  }

  private[mill] def deduplicate(items: List[T]): List[T] = items

  private[mill] def resolveRootModule(
      rootModule: BaseModule,
      scopedSel: Option[Segments]
  ): Result[BaseModule] = {
    scopedSel match {
      case None => Result.Success(rootModule)

      case Some(scoping) =>
        for {
          moduleCls <-
            try Result.Success(rootModule.getClass.getClassLoader.loadClass(scoping.render + "$"))
            catch {
              case e: ClassNotFoundException =>
                Result.Failure("Cannot resolve external module " + scoping.render)
            }
          rootModule <- moduleCls.getField("MODULE$").get(moduleCls) match {
            case rootModule: BaseModule => Result.Success(rootModule)
            case _ => Result.Failure("Class " + scoping.render + " is not an BaseModule")
          }
        } yield rootModule
    }
  }
}
